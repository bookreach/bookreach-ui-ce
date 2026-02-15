document.documentElement.setAttribute("data-theme", "light");

var app = Elm.Main.init({
    node: document.getElementById("elm"),
    flags: {
        prefecture: window.localStorage.getItem("prefecture") || "",
        coverSize: window.localStorage.getItem("explorerCoverSize") || "large"
    }
});

app.ports.saveToLocalStorage.subscribe(({ key, value }) => {
    window.localStorage.setItem(key, value);
});

app.ports.removeFromLocalStorage.subscribe((key) => {
    window.localStorage.removeItem(key);
});

app.ports.requestPrint.subscribe(req => {
    if (req) {
        window.print();
    }
});

app.ports.modalState.subscribe(opened => {
    if (opened) {
        document.documentElement.classList.add("is-clipped");
    } else {
        document.documentElement.classList.remove("is-clipped");
    }
});

// Unitrad
const unitradApi = "https://unitrad.calil.jp/v1";

async function unitradSearch(key, free = "", isbn = "", title = "", author = "", publisher = "", year_start = "", year_end = "", ndc = "") {
    const url = new URL(unitradApi + "/search");
    const params = {
        region: key,
        ndc,
        free,
        isbn,
        title,
        author,
        publisher,
        year_start,
        year_end
    };

    Object.entries(params).forEach(([paramName, paramValue]) => {
        if (paramValue) {
            url.searchParams.append(paramName, paramValue);
        }
    })

    try {
        const response = await fetch(url, { method: 'GET' });
        const data = await response.json();
        if (data.books === undefined) {
            data.books = [];
        }
        return attachNdcToBooks(ndc, data);
    } catch (error) {
        console.error('Error:', error);
        return null;
    }
}

async function unitradPolling(uuid, version) {
    const url = new URL(unitradApi + "/polling");
    url.searchParams.append("uuid", uuid);
    url.searchParams.append("version", version);
    url.searchParams.append("diff", 1);
    url.searchParams.append("timeout", 30);

    try {
        const response = await fetch(url, { method: 'GET' });
        const data = await response.json();
        return data;
    } catch (error) {
        console.error('Error:', error);
        return null;
    }
}

// Merge incremental polling results into the existing data object.
// Mutates `data` in place — intentional for efficiency in the polling loop.
// This matches the upstream implementation in bookreach-ui.
function mergeBookData(data, newData) {
    if (data.version === newData.version) {
        return;
    }
    let books_diff = newData["books_diff"];
    // Append newly discovered books
    Array.prototype.push.apply(data.books, books_diff.insert);
    // Update top-level metadata (version, running, count, etc.) but not books arrays
    for (let key in data) {
        if (data.hasOwnProperty(key) && !key.startsWith('books')) {
            data[key] = newData[key];
        }
    }
    // Patch individual book entries by index (_idx)
    for (let d of books_diff.update) {
        for (let key in d) {
            if (d.hasOwnProperty(key) && key !== '_idx') {
                if (Array.isArray(d[key]) === true) {
                    // Array fields (e.g. holdings): append new entries
                    Array.prototype.push.apply(data.books[d._idx][key], d[key]);
                } else if (d[key] instanceof Object) {
                    // Object fields: shallow-merge keys
                    for (let k in d[key]) {
                        if (d[key].hasOwnProperty(k)) {
                            data.books[d._idx][key][k] = d[key][k];
                        }
                    }
                } else {
                    // Scalar fields: overwrite
                    data.books[d._idx][key] = d[key];
                }
            }
        }
    }
}

async function unitradSearchByNdc(key, ndc) {
    return await unitradSearch(key, "", "", "", "", "", "", "", ndc);
}

function attachNdcToBooks(ndc, data) {
    for (let book of data.books) {
        book.ndc = ndc;
    }
    if (!Object.prototype.hasOwnProperty.call(data, "count")) {
        data["count"] = data.books.length
    }
    return data;
}

app.ports.requestUnitradByNdc.subscribe(async args => {
    let [regionKey, ndc] = args;
    let data = await unitradSearchByNdc(regionKey, ndc);
    if (data === null) {
        app.ports.receiveUnitradByNdc.send({
            uuid: "", version: 0,
            query: { region: regionKey, ndc: ndc },
            count: 0, books: [], running: false
        });
        return;
    }
    app.ports.receiveUnitradByNdc.send(data);
    while (data.running) {
        let newData = await unitradPolling(data.uuid, data.version);
        if (newData === null) {
            data.running = false;
        } else {
            mergeBookData(data, newData);
            attachNdcToBooks(ndc, data);
        }
        app.ports.receiveUnitradByNdc.send(data);
        await new Promise(r => setTimeout(r, 500));
    }
});

// Mapping
app.ports.requestMapping.subscribe(async regionKey => {
    try {
        const url = new URL(unitradApi + "/mapping");
        url.searchParams.append("region", regionKey);
        const response = await fetch(url, { method: 'GET' });
        const data = await response.json();
        app.ports.receiveMapping.send(data);
    } catch (error) {
        console.error('Mapping error:', error);
        app.ports.receiveMapping.send({ libraries: {} });
    }
});
