const path = require("path");

function resolve(relativePath) {
  return path.join(__dirname, relativePath);
}

module.exports = {
  entry: resolve("NavalFate.fsproj"),
  outDir: resolve("./build"),
  babel: {
    presets: [["@babel/env", { modules: "commonjs" }]],
    sourceMaps: true
  },
  fable: {
    define: ["DEBUG"]
  }
};
