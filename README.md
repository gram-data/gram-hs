# Gram HS

A multi-library Haskell mono-repo providing tools for working with patterns, subjects, and graph structures. The primary library, `pattern`, offers a generalized representation of graph elements using category theory.

## Quick Start

### Installation

**Prerequisites**:
- GHC 9.10.3
- Cabal 3.6.2.0 or later

**Using ghcup**:
```bash
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3
ghcup install cabal
ghcup set cabal
```

**Add to your project**:
```yaml
build-depends:
  - pattern
```

### Basic Usage

```haskell
import Pattern.Core (pattern, patternWith)

-- Create an atomic pattern
atomA = pattern "A"

-- Create a pattern with elements
atomB = pattern "B"
relationship = patternWith "knows" [atomA, atomB]
```

## Documentation

- **[User Documentation](docs/users/README.md)** - Complete user guide with examples
- **[Reference Documentation](docs/reference/PORTING-GUIDE.md)** - For language porters

## Libraries

- **`pattern`**: Core pattern data structure library (recursive, decorated sequences)
- **`subject`**: Special data structure with index, labels, and property record
- **`gram`**: Serialization/deserialization for "Subject Patterns"

## Project Structure

```
gram-hs/
├── libs/
│   ├── pattern/             # Pattern library
│   ├── subject/             # Subject library
│   └── gram/                # Gram library
├── docs/
│   ├── users/               # User-facing documentation
│   └── reference/           # Porter-facing documentation
└── apps/
    └── gram-hs-cli/         # CLI tool for testing and validation
```

## Design Goals

1. **Reference Implementation**: Canonical reference design for translation to other languages
2. **Category-Theoretic Foundation**: Built on solid mathematical foundations
3. **Schema-Lazy Design**: Patterns don't commit to specific graph semantics at creation time
4. **Multi-Language Translation**: Design prioritizes clarity and mathematical correctness

## Contributing

This is a reference implementation with strict quality standards. See `.specify/memory/constitution.md` for development principles.

**Workflow Requirements**:
- **ALWAYS work in a feature branch** - Never commit directly to `main`
- Use the Speckit workflow (`/speckit.specify`, `/speckit.plan`, `/speckit.tasks`)
- Follow the [Constitution](.specify/memory/constitution.md) principles

## License

BSD-3-Clause (see `LICENSE` file)
