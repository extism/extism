const { build } = require("esbuild");
const { dependencies, peerDependencies } = require('./package.json')

const sharedConfig = {
    entryPoints: ["src/index.ts"],
    bundle: true,
    minify: false,
    drop: [], // preseve debugger statements
    external: Object.keys(dependencies || {}).concat(Object.keys(peerDependencies || {})),
};

build({
    ...sharedConfig,
    platform: 'node', // for CJS
    outfile: "dist/index.js",
});

build({
    ...sharedConfig,
    outfile: "dist/index.esm.js",
    platform: 'neutral', // for ESM
    format: "esm",
});
