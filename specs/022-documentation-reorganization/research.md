# Research: Documentation Reorganization

**Feature**: 022-documentation-reorganization  
**Date**: 2025-01-27  
**Phase**: 0 - Research

## Research Questions

### 1. Documentation Organization for Multiple Audiences

**Question**: How should documentation be structured to serve distinct audiences (users vs. porters) without duplication?

**Decision**: Separate documentation hierarchies with clear entry points. Users start at `docs/users/README.md`, porters start at `docs/reference/PORTING-GUIDE.md`. Shared concepts (like architecture) go in reference docs and are linked from user docs when needed.

**Rationale**: 
- Clear separation prevents confusion
- Each audience has focused entry point
- Shared content linked, not duplicated
- Maintains single source of truth

**Alternatives Considered**:
- Single documentation with audience filters: Rejected - adds complexity, harder to navigate
- Duplicate content for each audience: Rejected - maintenance burden, inconsistency risk
- Tags/metadata approach: Rejected - requires tooling, less discoverable

### 2. Status Markers for Design Documents

**Question**: What format should be used to mark implementation status on design documents?

**Decision**: Use clear status headers at the top of each design document:
```markdown
**Status**: ✅ Implemented (Feature 23) | ⏳ Deferred | 📝 Design Only
**Reference**: See `docs/reference/features/graph-lens.md` for current spec
```

**Rationale**:
- Visual markers (✅, ⏳, 📝) are immediately recognizable
- Links to current documentation for implemented features
- Preserves historical context while indicating current state

**Alternatives Considered**:
- Separate status file: Rejected - requires maintenance, easy to get out of sync
- Metadata in frontmatter: Rejected - not visible in all viewers, requires parsing
- Status directory: Rejected - adds complexity, harder to discover

### 3. Historical Artifact Preservation

**Question**: How should historical development artifacts (specs/) be preserved while indicating they're not current?

**Decision**: Add `specs/README.md` explaining the historical nature. Optionally move to `docs/history/specs/` but preserve in place with clear markers. Each spec directory can have a status note.

**Rationale**:
- Preserves development history for context
- Clear markers prevent confusion
- Optional move to `docs/history/` provides cleaner organization
- README explains purpose without requiring changes to all specs

**Alternatives Considered**:
- Delete historical specs: Rejected - loses valuable development context
- Archive in separate repository: Rejected - harder to access, breaks links
- Convert all to current format: Rejected - too much work, loses historical accuracy

### 4. Porting Guide Structure

**Question**: What information should be included in the porting guide and in what order?

**Decision**: Porting guide should include:
1. Overview and purpose
2. Implementation phases with dependencies (Pattern → Subject → Gram)
3. Feature implementation order within each phase
4. Testing strategy using CLI tool
5. Language-specific considerations
6. Getting started checklist

**Rationale**:
- Phases match actual library dependencies
- Feature order reflects development sequence
- CLI tool integration is critical for validation
- Language considerations help porters adapt

**Alternatives Considered**:
- Alphabetical feature list: Rejected - doesn't show dependencies
- Single flat list: Rejected - doesn't show phase dependencies
- Technology-specific guides: Rejected - too many variations, maintenance burden

### 5. Documentation Link Management

**Question**: How should links be managed when reorganizing documentation?

**Decision**: 
- Use relative paths within `docs/` structure
- Update main README to link to new locations
- Add redirect notes in old locations pointing to new locations (if moving files)
- Use consistent link format: `docs/[section]/[file].md`

**Rationale**:
- Relative paths work across different viewers
- Redirect notes help during transition
- Consistent format makes links predictable
- Main README as central navigation point

**Alternatives Considered**:
- Absolute URLs: Rejected - breaks local viewing
- No redirects: Rejected - breaks existing bookmarks/links
- Automated link rewriting: Rejected - requires tooling, may break edge cases

### 6. CLI Tool Documentation Integration

**Question**: Where and how should CLI tool usage be documented for porters?

**Decision**: Include comprehensive CLI tool section in `docs/reference/PORTING-GUIDE.md` covering:
- Building the CLI tool
- Generating test cases
- Getting canonical reference outputs
- Validating implementations
- Round-trip testing workflows

**Rationale**:
- Porters need CLI tool for validation
- Centralized in porting guide where it's most relevant
- Comprehensive examples enable immediate use
- Testing workflows are critical for porters

**Alternatives Considered**:
- Separate CLI documentation: Rejected - porters need it integrated with porting workflow
- Minimal mention: Rejected - CLI tool is essential for validation
- Only in user docs: Rejected - users don't need porting validation tools

## Key Decisions Summary

1. **Separate documentation hierarchies** for users vs. porters
2. **Status markers** with visual indicators and links to current docs
3. **Preserve historical artifacts** with clear markers
4. **Phased porting guide** matching library dependencies
5. **Relative link management** with redirect notes
6. **Integrated CLI tool documentation** in porting guide

## Implementation Notes

- All decisions align with constitution principles
- No technology-specific choices (pure documentation)
- Focus on clarity and maintainability
- Preserve historical context while indicating current state
- Enable both audiences to find what they need quickly

