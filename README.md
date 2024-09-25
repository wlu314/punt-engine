<!-- omit in toc -->
# punt-engine: high-freq trading on FPGAs via Haskell

This project uses Clash (a semantic subset of Haskell) as a hardware description language to develop a high-frequency trading engine for FPGAs. 

<!-- omit in toc -->
# Table of Contents
- [Development](#development)

## Development

### Dependencies

We use [Nix](https://nixos.org/download/) to build and develop this project.

### Build/Test

Build the project with (first build will take eons):

```bash
nix-build
```

Verilog code will be available under the `result/share/verilog` directory.
Modify the `hdl` variable in `default.nix` to configure whether to generate
SystemVerilog or VHDL.

Streamline development with a Nix shell:

```
nix-shell
```

Then, to run the tests defined in `tests/`:

```bash
cabal run test-library
cabal run doctests
```

To compile the project to VHDL run:

```bash
cabal run clash -- Example.Project --vhdl
```

You can find the HDL files in `vhdl/`. The source can be found in `src/Example/Project.hs`.

Clash offers a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) much like Haskell's `ghci`:

```
cabal run clashi
```

Add custom dependencies or update nix via `niv`. See [niv on Hackage](https://hackage.haskell.org/package/niv) for more information.
