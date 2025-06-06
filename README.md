# pms-ui-request

`pms-ui-request` is one of the internal packages that make up the [`pty-mcp-server`](https://github.com/phoityne/pty-mcp-server) project.  
It is responsible for defining structured data types and utilities to represent and handle requests coming from the user interface layer, such as command-line inputs.

In the context of a stdio-mode MCP server, this package specifically handles the parsing and validation of JSON-RPC requests received via `stdin`.  
It acts as the entry point for transforming raw JSON input into well-typed commands that the application layer can interpret and execute.

---

## Package Structure
![Package Structure](https://raw.githubusercontent.com/phoityne/pms-ui-request/main/docs/11-1.png)
---

## Module Structure
![Module Structure](https://raw.githubusercontent.com/phoityne/pms-ui-request/main/docs/11-2.png)

---
