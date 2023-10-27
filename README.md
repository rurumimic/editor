# Editor

## Code

| lang | file | progress |
|---|---|---|
| C | [kilo.c](c/kilo.c) | ■■■___ |
| C++ | [main.cpp](cpp/main.cpp) | ■_____ |
| Go | [main.go](go/main.go) | ■■____ |
| Rust | [main.rs](rust/src/main.rs) | ■■■___ |
| Zig | [main.zig](zig/src/main.zig) | ■_____ |

## Build and Run

```bash
# c
make
./kilo kilo.c

# c++
make
./editor

# go
go run .

# rust
cargo run -- src/main.rs

# zig
zig build run
```

---

## Ref

- kilo: [github](https://github.com/antirez/kilo)
  - [Blog](http://antirez.com/news/108): Writing an editor in less than 1000 lines of code, just for fun
  - [Tutorial](https://viewsourcecode.org/snaptoken/kilo/): Build Your Own Text Editor
- [Avery Laird](https://www.averylaird.com/programming/the%20text%20editor/2017/09/30/the-piece-table): Text Editor - Data Structures
- [Cameron DaCamara](https://cdacamar.github.io/data%20structures/algorithms/benchmarking/text%20editors/c++/editor-data-structures/): Text Editor Data Structures
- [Tsoding Daily](https://youtu.be/2UY_Am-Q-oI): Writing Text Editor

