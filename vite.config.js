import { spawnSync } from "child_process";
import { defineConfig } from "vite";
import react from '@vitejs/plugin-react'

var alias = isDev()
  ? runMillCommand("js.publicDev")
  : runMillCommand("js.publicProd");

console.log("alias", alias);

export default defineConfig({
  server: {
    port: 7777,
    strictPort: true,
  },
  proxy: {
    '/api': {
      target: 'http://localhost:8080',
      rewrite: (path) => path.replace(/^\/api/, '')
    },
    '/ws': {
      target: 'ws://localhost:8080',
      rewrite: (path) => path.replace(/^\/ws/, ''),
      ws: true
    }
  },

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
