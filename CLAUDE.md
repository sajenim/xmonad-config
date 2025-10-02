# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal XMonad + Xmobar configuration built as a Haskell project using Nix flakes. The project uses Relude as a custom Prelude and follows a strict, opinionated Haskell development workflow.

## Build System

This project uses **just** for task running and **Nix flakes** for reproducible builds:

```bash
# List available commands
just

# Build the project
just build

# Run XMonad executable
just run xmonad

# Run XMobar executable
just run xmobar

# Start REPL
just repl

# Access Hoogle documentation (serves at http://127.0.0.1:8888)
just docs
```

### Nix Commands

```bash
# Update flake inputs
nix flake update

# Build all outputs
nix --accept-flake-config run github:juspay/omnix ci

# Run without installing
nix run .

# Enter development shell (or use direnv)
nix develop
```

### Pre-commit Hooks

Pre-commit hooks are automatically configured in the Nix shell and include:
- **fourmolu** for Haskell formatting (with FOURMOLU_DISABLE/ENABLE pragmas where needed)
- **nixpkgs-fmt** for Nix formatting
- **hlint** for Haskell linting

Run manually: `pre-commit run -a`

## Project Structure

The repository follows a standard Cabal package layout:

- **`src/xmonad.hs`** - Main XMonad configuration (executable entry point)
- **`src/xmobar.hs`** - XMobar status bar configuration (executable entry point)
- **`src/XMonadConfig/GruvboxMaterial.hs`** - Shared color scheme module providing xmobar escape codes

### Two Executables

The project builds two separate executables defined in `xmonad-config.cabal`:

1. **xmonad** - The window manager configuration
2. **xmobar** - The status bar configuration

Both share the `XMonadConfig.GruvboxMaterial` module for consistent theming.

## Architecture

### XMonad Configuration (`src/xmonad.hs`)

The configuration is built compositionally using function application:

```haskell
main = xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     . withNavigation2DConfig def
     . withSB myXmobar
     $ myConfig
```

**Key architectural components:**

- **Navigation2D**: Directional window and screen navigation using arrow keys and Navigation2D module
- **Custom keybindings**: Leader-key approach (`M-a` prefix) for spawning applications and layout switching
- **Window manipulation**: Uses `XMonad.Actions.RotSlaves` for window rotation (mimics wezterm behavior)
- **Layout system**: Three layouts - dynamic tiling, maximised, and fullscreen
- **StatusBar integration**: Communicates with xmobar via `_XMONAD_LOG_1` X property
- **Mnemonic bindings**: Control (M-C) for layout manipulation, Shift (M-S) for window operations

### XMobar Configuration (`src/xmobar.hs`)

Defines a status bar with:
- System information (uname, uptime, disk usage)
- Weather data (station YPJT)
- Date/time
- XMonad workspace/layout information from `_XMONAD_LOG_1` property

The bar is positioned statically for a multi-monitor setup (positioned at x=1920).

### Color Scheme Module

`XMonadConfig.GruvboxMaterial` exports color functions that wrap strings in xmobar color escape codes. This ensures consistent theming across both XMonad and XMobar configurations.

## Haskell Configuration

### Custom Prelude

Uses **Relude** instead of standard Prelude, configured in `xmonad-config.cabal`:

```cabal
mixins:
  base hiding (Prelude),
  relude (Relude as Prelude, Relude.Container.One),
  relude
```

### Default Extensions

The following extensions are enabled project-wide:
- DataKinds, DerivingStrategies, DerivingVia
- LambdaCase, MultiWayIf, NoStarIsType
- OverloadedStrings, StrictData, TypeFamilies, ViewPatterns

### GHC Options

Strict compilation with extensive warnings:
- `-Wall`
- `-Wincomplete-record-updates`, `-Wincomplete-uni-patterns`
- `-Wmissing-deriving-strategies`, `-Wunused-foralls`
- `-fprint-explicit-foralls`, `-fprint-explicit-kinds`

## Development Workflow

1. The project uses **haskell-flake** via flake-parts for Haskell package management
2. HLS (Haskell Language Server) is available but checks are disabled in devShell (`hlsCheck.enable = false`)
3. Static analysis via **stan** is enabled for the xmonad-config package
4. The project filters `projectRoot` in `nix/modules/flake/haskell.nix` to avoid unnecessary rebuilds

## Key Dependencies

- **xmonad**, **xmonad-contrib** - Core window manager libraries
- **xmobar** - Status bar library
- **relude** - Alternative Prelude
- **optics-core** - For lens-style accessors
- **data-default** - Default instances for configuration
