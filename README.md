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
- **User Documentation**: `docs/` - User-facing guides and API documentation
  - [Pattern Construction Functions](docs/users/api/pattern-construction.md) - Guide to `point`, `pattern`, and `pure` with porting guidance
- **Design Documentation**: `design/DESIGN.md` - Category-theoretic framework and design principles
- **Implementation Roadmap**: `TODO.md` - Planned features and implementation phases
- **Feature Specifications**: `specs/` - Detailed specifications for each feature
- **API Documentation**: Generate with `cabal haddock` or see `specs/*/contracts/type-signatures.md`
- **Quickstart Guide**: `specs/002-basic-pattern-type/quickstart.md`

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
- **Typeclass Instances**: Show ✅, Eq ✅, Functor ✅, Foldable ✅, Traversable (Phase 2-6)
- **Constructor Functions**: `point` (atomic) and `pattern` (with elements) (Phase 3)
- **Classification Functions**: `isNode`, `isRelationship`, `isSubgraph`, `isPath` (Phase 8-11)
- **Navigation Functions**: `source`, `target`, `nodes`, `relationships` (Phase 12)
- **Graph Views**: `GraphView` typeclass and implementations (Phase 15)
- **Query Functions**: `length`, `size`, `depth` (Phase 7)

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
