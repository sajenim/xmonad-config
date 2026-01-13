# XMonad Configuration

Personal XMonad window manager configuration with XMobar status bar, built as a Haskell project using Nix flakes.

## Features

- **Modal Keybindings**: i3wm-inspired modal organization for layouts and application spawning
- **Leader Key Approach**: `M-a` prefix for commands and mode entry
- **Gruvbox Material Theme**: Consistent color scheme across XMonad and XMobar
- **Multi-monitor Support**: XMobar positioned for dual-monitor setup
- **Functional Architecture**: Compositional configuration using Relude as custom Prelude

## Quick Start

```bash
# List available commands
just

# Build the project
just build

# Enter development shell (or use direnv)
nix develop

# Access documentation (serves at http://127.0.0.1:8888)
just docs
```

## Project Structure

```
src/
├── xmonad.hs                        # Window manager configuration
├── xmobar.hs                        # Status bar configuration
└── XMonadConfig/
    └── GruvboxMaterial.hs          # Shared color scheme module
```

Two executables are built:
- **xmonad** - Window manager
- **xmobar** - Status bar

Both share the `XMonadConfig.GruvboxMaterial` module for consistent theming.

## Key Bindings

### Modal Keybindings

- **Layout Mode** (`M-a l`):
  - `d/m/f` - Jump to layout (exits immediately)
  - Arrow keys - Adjust master/slave split (stays in mode, `<Escape>` to exit)

- **Spawn Mode** (`M-a s`):
  - `t` - Thunar file manager
  - `s` - Screenshot tool

### Window Management

- Window rotation using `XMonad.Actions.RotSlaves` (mimics wezterm behavior)
- Three layouts: dynamic tiling, maximised, fullscreen

## Development

### Build System

Uses **just** for task running and **Nix flakes** for reproducible builds:

```bash
# Update flake inputs
nix flake update

# Build all outputs
nix --accept-flake-config run github:juspay/omnix ci

# Start REPL
just repl
```

### Pre-commit Hooks

Automatically configured in the Nix shell:
- **fourmolu** - Haskell formatting
- **nixpkgs-fmt** - Nix formatting
- **hlint** - Haskell linting

Run manually: `pre-commit run -a`

### Haskell Configuration

- **Custom Prelude**: Uses Relude instead of standard Prelude
- **Strict compilation**: Extensive warnings enabled (`-Wall`, incomplete pattern warnings, etc.)
- **Default Extensions**: DataKinds, DerivingStrategies, LambdaCase, OverloadedStrings, and more
- **Static Analysis**: Stan enabled for the xmonad-config package

## Dependencies

- **xmonad**, **xmonad-contrib** - Core window manager libraries
- **xmobar** - Status bar library
- **relude** - Alternative Prelude
- **optics-core** - Lens-style accessors
- **data-default** - Default instances for configuration

## Architecture

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

StatusBar integration communicates via `_XMONAD_LOG_1` X property, displaying active modes and workspace information.
