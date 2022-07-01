# BookReach UI (Community Edition)

[English](./README.md)

学校図書館による教材図書提供を支援するアプリケーション BookReach のユーザインタフェース（UI）．

> 「授業で本を使いたいが、本校でどんな本が使えるかわからない。」
>
> 「先生に頼まれて図書リストを作っているが、授業内容がよくわからない。」

学校図書館を活用できる場面で、専門性の違いから生まれる難しさをサポートします．

![BookReach-UIの画面イメージ ](/assets/bookreach-ui.png)

## 動作確認

[https://bookreach.github.io/bookreach-ui/](https://bookreach.github.io/bookreach-ui/) をブラウザで開いてください．

### UI の操作方法

1. 校種を選ぶ（高校には非対応）
2. 教科書を選ぶ
3. 支援予定の授業で扱う単元を選ぶ
4. 推薦図書の候補を選ぶ
   1. 書影をクリックすると，詳細が開くので，中身を確認する
   2. 使えそうな本であれば「選択」ボタンをクリックする
5. ページ最下部にリストが自動作成される
6. 必要に応じてリストを印刷したり，テキストファイルに書き出して Excel 等でさらに編集する

その他の機能など，詳細は下記論文をご覧ください．

## 開発

> 本リポジトリは下記論文で提案されたシステムの参考実装です．
> 機能やコード構成など，まだまだ改善点がありますので，プルリクエスト大歓迎です！

開発を進めるためには，最低限下記のものをインストールする必要があります．

- [Elm (0.19.1)](https://elm-lang.org/)
- [`node.js`](https://nodejs.org/en/)

> Elm は Web アプリケーション開発に適した素晴らしい関数型言語です．
> 関数型と言ってもかなりわかりやすいシンプルな仕様になっていて，着実に自信を持って，それでいて楽しくコーディングできておすすめです．
> ご存じない方は公式サイトのチュートリアルなどご覧いただけたらと思います．
>
> BookReach UI のコードは Elm で閉じていて，npm パッケージ等には依存していません．
> 最近の（移り変わりが激しい）web 開発ツールに関する知識は不要です．

まず，プロジェクトに必要なツールをインストールします．

```bash
npm install  # Elm関連の開発ツールをインストールします
elm install  # 本Elmコードが使用するElmパッケージをインストールします
```

`./src` 以下の Elm コードを変更するたびに，以下を実行します．

```shell
elm make src/Main.elm --debug --output=main.js  # ElmコードをJSにコンパイルします
```

`./index.html` を開いていただくと，動作するアプリが表示されると思います．
画面右下の Elm ロゴマークをクリックすると，便利な Elm デバッガーが立ち上がり，ステップごとの Elm モデル更新の様子がわかります（Elm の公式機能です！）．
プロダクション環境にデプロイするときは，コンパイルオプションの `--debug` を外します．

[`elm-live`](https://github.com/wking-io/elm-live) という開発用 web サーバを使うと，上記のコンパイルとアプリのリロードをファイル変更に合わせて自動で実施してくれて便利です．
npm パッケージに指定してあるので，上記手順を踏んでいればそのまま使えます．

```bash
npx elm-live src/Main.elm --start-page=index.html -- --output=main.js --debug
```

### Misc. info

- [Bulma](https://bulma.io) CSS を UI ブロックに使用しています． `./index.html` で CDN から直接読み込んでいます (npm パッケージとして取り込んではいません)
- 図書データベースの API は [`json-server`](https://github.com/typicode/json-server) の仕様に準拠していることを前提としています．
  - [Heroku](https://heroku.com) 上の[サンプル API](https://sample-bookdb.herokuapp.com/) は少数の図書データをサンプルとして返戻しますが， 著作権の問題でいくつかのフィールドについてランダム値を挿入しています．図書や教科書などに関するこれらデータの正しさは全く保証しないことをご承知おきください．
  - サンプル API は Heroku の無料枠で運用しており，起動に数秒かかります
  - ご自身で図書データベースを作成し，`json-server` で読み込んでローカル開発できます
  - 図書データベースの仕様・要件は `./src/BookDB.elm` から読み取っていただけると思います
- 個人的には，VSCode に [Elm extension](https://github.com/elm-tooling/elm-language-client-vscode) をインストールして開発しています
- Safari と Brave ブラウザで動作確認していますが，厳密なテストは実施していません

## Citations

このソフトウェアを利用した際は下記論文の引用をお願いします．

```bibtex
@ARTICLE{Yada2020,
  title     = "学校図書館による教材提供を支援する図書選定システムの提案とユーザインタフェースの予備的評価",
  author    = "矢田, 竣太郎 and 浅石, 卓真 and 宮田, 玲",
  journal   = "日本図書館情報学会研究大会発表論文集",
  publisher = "日本図書館情報学会",
  volume    =  68,
  pages     = "9--12",
  year      =  2020
}
```

`./CITATION.cff` や `./CITATIONS.bib` もご覧ください．
英語版や雑誌論文の書誌情報もあります．

## ライセンス

MIT (`./LICENCE`)

## 開発者

[矢田 竣太郎](https://shuntaroy.com)
