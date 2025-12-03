# swatson555-blog-compiler

**A static site generator for [swatson555.github.io](https://swatson555.github.io).**

## Description
This project is a static site generator built with **Haskell** and **Hakyll**. It handles the compilation of Markdown source files into a static HTML website, enforcing strict separation between content, logic, and presentation.

## Table of Contents
* [Architecture](#architecture)
* [Usage](#usage)
* [Project Structure](#project-structure)

## Architecture
The build graph transforms source content into deployable artifacts through a unidirectional pipeline.

```mermaid
flowchart LR
    A([Source: Markdown]) --> B(Pandoc Reader)
    B -->|AST| C(Templating)
    C --> D([Final Artifact])
```

## Usage

### Prerequisites
* **[Haskell Stack](https://docs.haskellstack.org/en/stable/README/)**
* **Git**

### Build & Run
```bash
# 1. Build the compiler binary (downloads GHC/Dependencies)
stack build

# 2. Compile content and start local server (auto-reloads)
stack exec blog watch
# -> [http://127.0.0.1:8000](http://127.0.0.1:8000)
```

### Create a Post
Add a new Markdown file to the `posts/` directory. Refer to existing files in that folder for the required YAML metadata structure.

## Project Structure
* **`site.hs`**: Build rules and routing logic.
* **`posts/`**: Source content (Markdown).
* **`templates/`**: HTML layout definitions.
* **`css/`**: SASS/CSS assets.
* **`stack.yaml`**: Dependency resolver configuration.
