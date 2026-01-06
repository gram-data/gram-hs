# Verification Checklist: Documentation Reorganization

**Feature**: 027-docs-reorganization  
**Date**: 2025-01-28

## Purpose

This checklist verifies that the documentation reorganization is complete and correct. All items must pass before considering the feature complete.

## File Moves Verification

### User Guide Directory Move

- [ ] `docs/guide/` directory exists
- [ ] `docs/guide/01-introduction.md` exists
- [ ] `docs/guide/02-basic-concepts.md` exists
- [ ] `docs/guide/03-construction.md` exists
- [ ] `docs/guide/04-basic-operations.md` exists
- [ ] `docs/guide/05-typeclass-instances.md` exists
- [ ] `docs/guide/06-advanced-morphisms.md` exists
- [ ] `docs/guide/07-use-cases.md` exists
- [ ] `docs/guide/08-gram-notation-reference.md` exists
- [ ] All 8 guide files accessible (total count = 8)

**Verification Command**:
```bash
ls docs/guide/*.md | wc -l
# Expected: 8
```

### Reference File Moves

- [ ] `docs/reference/migration/` directory exists
- [ ] `docs/reference/migration/rename-constructors.md` exists
- [ ] `docs/reference/features/pattern-construction.md` exists

**Verification Command**:
```bash
test -f docs/reference/migration/rename-constructors.md && echo "✓" || echo "✗"
test -f docs/reference/features/pattern-construction.md && echo "✓" || echo "✗"
```

## Directory Deletions Verification

### Redundant Directories

- [ ] `docs/users/guides/` directory does not exist
- [ ] `docs/users/examples/` directory does not exist
- [ ] `docs/users/api/` directory does not exist
- [ ] `docs/users/migration/` directory does not exist
- [ ] `docs/users/` directory does not exist

**Verification Command**:
```bash
test ! -d docs/users/guides && echo "✓" || echo "✗"
test ! -d docs/users/examples && echo "✓" || echo "✗"
test ! -d docs/users/api && echo "✓" || echo "✗"
test ! -d docs/users/migration && echo "✓" || echo "✗"
test ! -d docs/users && echo "✓" || echo "✗"
```

### Redundant README Files

- [ ] `docs/users/guide/README.md` does not exist
- [ ] `docs/users/README.md` does not exist
- [ ] `docs/reference/README.md` does not exist
- [ ] `docs/design/README.md` does not exist
- [ ] `docs/README.md` still exists (main index)

**Verification Command**:
```bash
test ! -f docs/users/guide/README.md && echo "✓" || echo "✗"
test ! -f docs/users/README.md && echo "✓" || echo "✗"
test ! -f docs/reference/README.md && echo "✓" || echo "✗"
test ! -f docs/design/README.md && echo "✓" || echo "✗"
test -f docs/README.md && echo "✓" || echo "✗"
```

## Link Updates Verification

### Root README.md

- [ ] Root `README.md` link points to `docs/guide/01-introduction.md`
- [ ] No references to `docs/users/README.md` in root README.md

**Verification Command**:
```bash
grep -n "docs/guide/01-introduction.md" README.md
grep -n "docs/users/README.md" README.md
# First should find link, second should find nothing
```

### Guide File Links

- [ ] No references to `docs/users/` in guide files
- [ ] No references to `../users` in guide files
- [ ] Internal guide links (e.g., `[Introduction](01-introduction.md)`) remain valid
- [ ] Links to reference docs use correct relative paths

**Verification Command**:
```bash
grep -r "docs/users" docs/guide/ || echo "✓ No old path references"
grep -r "\.\./users" docs/guide/ || echo "✓ No old relative path references"
```

### Reference Documentation Links

- [ ] `docs/reference/PORTING-GUIDE.md` link to user docs updated
- [ ] `docs/reference/ARCHITECTURE.md` links updated (if any)
- [ ] `docs/reference/migration/rename-constructors.md` links updated (if any)
- [ ] `docs/reference/features/pattern-construction.md` links updated (if any)
- [ ] No references to deleted README files

**Verification Command**:
```bash
grep -n "docs/users" docs/reference/PORTING-GUIDE.md docs/reference/ARCHITECTURE.md docs/reference/migration/rename-constructors.md docs/reference/features/pattern-construction.md || echo "✓ No old path references"
grep -r "README.md" docs/reference/ | grep -v "PORTING-GUIDE\|ARCHITECTURE\|SPECIFICATION\|IMPLEMENTATION" || echo "✓ No references to deleted README files"
```

## Git History Verification

### File History Preservation

- [ ] All moves used `git mv` (check git status shows renames, not deletes+adds)
- [ ] Git log shows file history preserved

**Verification Command**:
```bash
git status
# Should show "renamed" for moved files, not "deleted" + "new file"
git log --follow --oneline docs/guide/01-introduction.md | head -5
# Should show commit history
```

## Structure Verification

### Final Directory Structure

- [ ] `docs/guide/` contains only numbered guide files (no README.md)
- [ ] `docs/reference/` contains reference docs and subdirectories
- [ ] `docs/reference/migration/` contains migration guides
- [ ] `docs/reference/features/` contains feature specs including pattern-construction.md
- [ ] Directory structure is clean and self-explanatory

**Verification Command**:
```bash
ls docs/guide/
# Should show only: 01-introduction.md through 08-gram-notation-reference.md
ls docs/reference/
# Should show: PORTING-GUIDE.md, ARCHITECTURE.md, SPECIFICATION.md, IMPLEMENTATION.md, migration/, features/, semantics/
```

## Link Resolution Verification

### Manual Link Checks

- [ ] Root README.md link to guide resolves correctly
- [ ] Guide section 01 links to 02 correctly
- [ ] Guide section 02 links to 03 correctly
- [ ] Guide section 03 links to 04 correctly
- [ ] Guide section 04 links to 05 correctly
- [ ] Guide section 05 links to 06 correctly
- [ ] Guide section 06 links to 07 correctly
- [ ] Guide section 07 links to 08 correctly
- [ ] PORTING-GUIDE.md link to user docs resolves correctly
- [ ] No broken relative links in any markdown files

**Note**: Manual verification required - click through links or use markdown link checker

## Summary

**Total Checks**: ~50 verification items  
**Critical Checks**: File moves, directory deletions, root README link update  
**Optional Checks**: Link resolution (can be verified manually)

## Completion Criteria

✅ All file moves verified  
✅ All directory deletions verified  
✅ All README deletions verified  
✅ All link updates verified  
✅ Git history preserved  
✅ Structure matches plan  
✅ No broken links  

**Status**: [ ] Complete [ ] In Progress [ ] Blocked

