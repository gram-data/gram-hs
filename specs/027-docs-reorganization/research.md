# Research: Documentation Reorganization

**Feature**: 027-docs-reorganization  
**Date**: 2025-01-28

## Research Questions

### 1. Git Move Best Practices

**Question**: How to preserve git history when moving files and directories?

**Research Findings**:

- **Decision**: Use `git mv` for all file and directory moves
- **Rationale**: 
  - `git mv` preserves file history and shows moves in git log
  - Better than `mv` + `git add` which can lose history context
  - Git tracks renames automatically when similarity threshold is met
  - For directories: `git mv old-dir/ new-dir/` moves entire directory tree
- **Alternatives Considered**:
  - `mv` + `git add`: Loses rename tracking, creates delete + add in history
  - `git mv` with intermediate steps: Unnecessary complexity
- **Implementation**: Use `git mv` for all moves, including directory moves

### 2. Link Verification Strategy

**Question**: How to verify all markdown links resolve correctly after reorganization?

**Research Findings**:

- **Decision**: Use combination of manual verification and grep-based link checking
- **Rationale**:
  - Markdown links use relative paths: `[text](path/to/file.md)`
  - Need to check all `.md` files for link patterns
  - Verify links resolve to existing files after moves
  - Check for broken references to deleted files/directories
- **Alternatives Considered**:
  - Automated link checker tools: May not handle relative paths correctly in all cases
  - Manual checking only: Prone to errors, but necessary for verification
- **Implementation**: 
  - Use `grep -r` to find all markdown links
  - Manually verify critical links (README.md, cross-references)
  - Check for references to old paths (`docs/users/`, deleted README files)

### 3. Directory Move Strategy

**Question**: How to move entire directory trees while preserving structure?

**Research Findings**:

- **Decision**: Use `git mv` for directory moves, create target directories if needed
- **Rationale**:
  - `git mv docs/users/guide docs/guide` moves entire directory
  - Git automatically tracks directory renames
  - Need to ensure target parent directory exists first
  - For nested moves: `git mv docs/users/migration docs/reference/migration`
- **Alternatives Considered**:
  - Moving files individually: More operations, but preserves history per file
  - Using `mv` then `git add`: Loses rename tracking
- **Implementation**: 
  - Move directories with `git mv` when entire directory moves
  - Create target directories first if they don't exist
  - Verify directory structure after moves

### 4. Link Update Patterns

**Question**: What link patterns need updating after file moves?

**Research Findings**:

- **Decision**: Update relative paths in markdown links, preserve absolute references
- **Rationale**:
  - Relative links: `[text](../guide/01-introduction.md)` need path updates
  - Links within same directory: `[text](02-basic-concepts.md)` remain unchanged
  - Links to moved files: Need path updates based on new location
  - Links to deleted files: Must be removed or updated to new location
- **Alternatives Considered**:
  - Using absolute paths from repo root: More verbose, but clearer
  - Using relative paths: Standard markdown practice, but requires updates
- **Implementation**:
  - Update relative paths in all guide files
  - Update cross-references in reference docs
  - Remove links to deleted README files
  - Update root README.md link to new guide location

### 5. Verification Approach

**Question**: How to ensure reorganization is complete and correct?

**Research Findings**:

- **Decision**: Multi-step verification process
- **Rationale**:
  - Verify all files moved successfully (check target locations exist)
  - Verify all files deleted successfully (check source locations don't exist)
  - Verify all links resolve (check markdown link patterns)
  - Verify directory structure matches plan
- **Alternatives Considered**:
  - Single verification step: May miss issues
  - Automated testing: Overkill for documentation reorganization
- **Implementation**:
  - File existence checks: `ls` or `test -f` for moved files
  - Directory structure check: `ls -R` to verify structure
  - Link pattern check: `grep -r` for markdown links
  - Manual spot-check of critical links

## Summary

All research questions resolved. Key decisions:
1. Use `git mv` for all file and directory moves to preserve history
2. Manual verification + grep-based link checking for link verification
3. Move directories as units using `git mv`
4. Update relative paths in markdown links based on new structure
5. Multi-step verification process to ensure completeness

No blocking issues identified. Ready to proceed with implementation plan.

