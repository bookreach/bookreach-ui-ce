# BookReach UI (Community Edition)

[English](./README.md)

学校図書館による教材図書提供を支援するアプリケーション BookReach のユーザインタフェース（UI）．

> 「授業で本を使いたいが、本校でどんな本が使えるかわからない。」
>
> 「先生に頼まれて図書リストを作っているが、授業内容がよくわからない。」

学校図書館を活用できる場面で、専門性の違いから生まれる難しさをサポートします．

## 動作確認

[https://bookreach.github.io/bookreach-ui-ce/](https://bookreach.github.io/bookreach-ui-ce/) をブラウザで開いてください．

### 使い方

1. 学校図書館がある都道府県を選ぶ
2. 校種・学年・教科を選び，授業のテーマをフリーテキストで入力する
3. NDL（国立国会図書館）の予測 API が関連する NDC（日本十進分類法）コードを提案する
4. 「図書を検索」ボタンで都道府県内の図書館蔵書をカーリル Unitrad API で検索する
5. NDC タブごとに結果を閲覧し，書誌詳細や所蔵館を確認する
6. 使えそうな本を選び，CSV・TSV・印刷でリストを書き出す

## アーキテクチャ

バックエンドサーバは不要で，公開 API のみを利用するシングルページアプリケーションです．

| API | 用途 |
|-----|------|
| [カーリル Unitrad](https://calil.jp/doc/api_ref.html) | NDC コードによる図書館蔵書検索 |
| [NDL 予測 API](https://lab.ndl.go.jp/ndc/) | フリーテキストから NDC コードを予測 |
| [openBD](https://openbd.jp/) | 書影画像 |

アプリは3つのステージで構成されています：

1. **都道府県選択** — 都道府県を選択（localStorage に保存）
2. **NDC 選択** — 校種・教科・学年を選び，テーマを入力して予測された NDC コードを選択
3. **エクスプローラ** — 図書の閲覧・詳細表示・フィルタ・選択・書き出し

## 開発

必要な環境：

- [Node.js](https://nodejs.org/en/)（v18 以上）
- [Elm](https://elm-lang.org/)（0.19.1，npm 経由でインストール）

### セットアップ

```bash
npm install          # 依存パッケージをインストール（Elm, elm-watch, Sass 等）
npm run build-bulma  # Bulma SCSS を CSS にコンパイル
```

### コマンド一覧

| コマンド | 説明 |
|---------|------|
| `npm start` | ホットリロード付き開発サーバを起動（ポート 3000） |
| `npm run build-bulma` | `br-bulma.scss` を `public/br-bulma.css` にコンパイル |
| `npm test` | Elm テストを実行 |
| `npm run format` | elm-format で Elm ソースコードを整形 |

### プロジェクト構成

```
src/
  Main.elm          # エントリポイント（3ステージ構成のエクスプローラ）
  Api.elm           # 型定義・デコーダ・HTTP関数
  NdcSelect.elm     # NDC選択コンポーネント（フリーテキスト→NDL予測）
  BookFilter.elm    # クエリ・図書館フィルタ
  School.elm        # 校種・教科・学年の定義
  Utils.elm         # LocalStore・ヘルパー関数

public/
  index.html        # HTML シェル
  custom.js         # Unitrad検索・ポーリング，マッピング，ポート
  custom.css        # カスタムスタイル
  data/
    prefectures.json   # 47都道府県データ
    ndc9-lv3.json      # NDC3次区分ラベル

br-bulma.scss       # Bulma CSS 設定
```

### 技術スタック

- **言語**: [Elm](https://elm-lang.org/) 0.19.1
- **CSS フレームワーク**: [Bulma](https://bulma.io) 1.0.1（SCSS 経由）
- **開発サーバ**: [elm-watch](https://lydell.github.io/elm-watch/)（ホットリロード対応）
- **アイコン**: [Font Awesome](https://fontawesome.com/) 6（CDN）

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
