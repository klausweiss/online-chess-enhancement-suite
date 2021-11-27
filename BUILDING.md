# Building online-chess-enhancement-suite

Once you have development environment setup, building the bundle for distribution should be as simple as running:

```sh
npm run dist
```


To setup the development environment it should be enough to run:
```sh
npm install --include=dev
```


## Development

Build the development version with:

```sh
npm run dev
```

This will create a `distribution/webext-dev/` folder.
Open the browser config page where you debug extensions and load the extension from this folder.

