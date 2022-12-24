import { spawnSync } from "child_process";
import { defineConfig } from "vite";
import react from '@vitejs/plugin-react'

var alias = isDev()
  ? runMillCommand("debate.js.publicDev")
  : runMillCommand("debate.js.publicProd");

console.log("alias", alias);

export default defineConfig({
    plugins: [react()],
  resolve: {
        alias: alias,
  },
});

function isDev() {
  return process.env.NODE_ENV !== "production";
}

function runMillCommand(command) {
  const result = spawnSync("mill", ["show", command], {
    stdio: [
      "pipe", // StdIn.
      "pipe", // StdOut.
      "inherit", // StdErr.
    ],
  });

  return JSON.parse(result.stdout);
}
