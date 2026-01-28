# service_box

Collection of crates for building rust services.

Everything in this repository is already running in production services, but it's still in active development so expect breaking changes.

## Crates

### arktype_ast
Core data model for representing ArkType schemas, including an expression AST, dependency tracking, and rendering helpers used by other crates that generate bindings.

### arktype_ast_macros
Proc-macro crate that implements the `ArkType` derive, turning Rust structs and enums into schema bindings for [ArkType](https://arktype.io/)-based type generation.

### arktype_ast_test
Integration test crate that exercises the `ArkType` derive and validates the generated schema bindings for a wide range of Rust type shapes.

### cfg
Small configuration loader that deserializes TOML files into strongly typed structs and provides clear read/parse errors for missing or invalid configs.

### errx
Structured error type with trace IDs, stack location capture, and Axum response integration, plus helpers for attaching error context to tracing spans.

### pg
Postgres pooling utility built on `deadpool-postgres` that reads connection settings from config and exposes shared pool/client access for services.

### rpc
RPC framework built on Axum and `inventory` that registers endpoints, wires handlers, and generates TypeScript bindings from ArkType schemas.

### rpc_attr
Proc-macro attribute for RPC handlers that validates function signatures and expands to the `rpc` wiring macros.

### rpc_test
Integration test crate that validates the RPC macros, router registration, and binding generation behavior for the `rpc` crate.

### sf
Sonyflake-based ID type with serde support and optional Postgres conversions for BIGINT storage while keeping JSON serialization string-safe.

### tracing_provider
Tracing subscriber setup with configurable file logging, optional Axiom export, and task-local error context propagation for correlated logs.
