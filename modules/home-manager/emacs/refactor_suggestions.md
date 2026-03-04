# Emacs Configuration Refactor Suggestions

Based on a review of the configuration in `modules/home-manager/emacs/`, here are some suggestions for updating deprecated plugins and modernizing the architecture.

## 1. Deprecated or Archived Plugins (High Priority)

These plugins are either archived, unmaintained, or have known issues:

*   ~~**`peep-dired`** *(in `dired.nix`)*~~ **(DONE)**
    *   ~~**Status:** Archived and abandoned.~~
    *   ~~**Alternative:** **`dired-preview`**. A modern, actively maintained package by Protesilaos Stavrou that does exactly what `peep-dired` did, but faster and less prone to glitches.~~
*   ~~**`neotree`** *(in `navigation_and_shell.nix`)*~~ **(REMOVED)**
    *   ~~**Status:** Largely unmaintained and can be slow compared to modern alternatives.~~
    *   ~~**Alternative:** Decided to completely remove the sidebar file explorer in favor of fuzzy-finding and standard full-screen Dired buffers.~~
*   ~~**`undo-tree`** *(in `evil.nix`)*~~ **(DONE)**
    *   ~~**Status:** Not actively maintained and notorious for corrupting undo history files on disk.~~
    *   ~~**Alternative:** **`undo-fu`** (for the linear undo/redo API used by Evil) combined with **`vundo`** (for the visual tree representation). Emacs 28+ also has built-in `undo-redo`.~~
*   ~~**`org-bullets`** *(in `ui.nix`)*~~ **(DONE)**
    *   ~~**Status:** Has not been updated in years.~~
    *   ~~**Alternative:** **`org-superstar`** is its direct, maintained successor. Alternatively, **`org-modern`** is the new standard for making org-mode look incredibly sleek overall.~~

## 2. Architectural Modernizations (The "New Generation" Stack)

The Emacs community has overwhelmingly shifted away from heavy, monolithic frameworks from 2016 toward lightweight, modular plugins that hook directly into Emacs's native C-engine APIs (Emacs 28+). 

*   **Search/Select Framework:** `ivy` / `counsel` / `prescient` ➔ **`vertico`** / **`consult`** / **`orderless`** / **`marginalia`** *(in `search_and_select.nix`)*
    *   **Why:** `ivy` and `counsel` bypass native Emacs APIs and require custom integrations for every third-party plugin. `vertico` simply makes the *native* Emacs `completing-read` API look beautiful, meaning *any* plugin works out of the box. `orderless` provides space-separated fuzzy matching, and `marginalia` adds file paths/descriptions to the menus.
    *   *Note on Spelling:* This also requires swapping `flyspell-correct-ivy` back to the default `flyspell-correct` (which natively hooks into Vertico's UI).
*   ~~**In-Buffer Autocomplete:** `company` / `company-box` ➔ **`corfu`** *(in `completion.nix`)*~~ **(DONE)**
    *   ~~**Why:** Yes, `company` (specifically `company-box`) is 100% responsible for the severe visual lag, flickering, and the annoying popup that forces you to press `ESC` when typing normal words. `corfu` uses modern Emacs drawing APIs, making it lightning fast, non-intrusive, and much easier to configure (e.g., to only trigger on `TAB` or have a higher delay so it stops interrupting your flow).~~
*   **Icons:** `all-the-icons` ➔ **`nerd-icons`** *(in `ui.nix`)*
    *   **Why:** `all-the-icons` is essentially abandoned. `nerd-icons` maps directly to the standard Nerd Fonts you use in Neovim and terminal, has a vastly larger library, and is the default for all modern Emacs packages (like Doom Modeline, Corfu, Marginalia).
*   **Project Management:** `projectile` ➔ **`project.el`** *(in `core.nix`)*
    *   **Why:** As of Emacs 28, `project.el` is built directly into the core editor. It does exactly what Projectile does (finding git roots, searching within projects) but requires zero extra packages to download and integrates flawlessly with standard tools.

## 3. Workflow Improvements
*   ~~**Copy inside buffer:** Ensure there are ergonomic bindings for copying the entire buffer or specific text objects efficiently.~~ **(DONE)** Implemented Neovim-style `<leader> y` OS yanking operators and clipboard isolation.

## Notes
*   The `org-roam` configuration is excellent, particularly the custom functions for handling file renaming and image cleanup.
*   The use of `lib.mkOrder` to manage the evaluation sequence (`core.nix`, `evil.nix`, `keybindings.nix`) is a very solid architectural choice.
