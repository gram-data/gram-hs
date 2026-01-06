# Quickstart: Documentation Reorganization

**Feature**: 027-docs-reorganization  
**Date**: 2025-01-28

## Overview

This quickstart guide provides step-by-step instructions for reorganizing the documentation structure. All operations use `git mv` to preserve file history.

## Prerequisites

- Git repository with documentation structure from feature 026-pattern-documentation
- All files in `docs/users/guide/` exist
- Reference documentation structure exists

## Step-by-Step Instructions

### Phase 1: Move User Guide Directory

**Goal**: Move `docs/users/guide/` to `docs/guide/`

```bash
# Move the entire guide directory to top level
git mv docs/users/guide docs/guide

# Verify move succeeded
ls docs/guide/
# Should show: 01-introduction.md through 08-gram-notation-reference.md
```

**Expected Result**: All 8 numbered guide files are now at `docs/guide/`

### Phase 2: Create Reference Subdirectories

**Goal**: Ensure target directories exist for reference file moves

```bash
# Create migration subdirectory if it doesn't exist
mkdir -p docs/reference/migration

# Verify features directory exists (should already exist)
ls docs/reference/features/
```

**Expected Result**: `docs/reference/migration/` directory exists

### Phase 3: Move Reference Files

**Goal**: Move migration and API porting guides to reference directory

```bash
# Move migration guide
git mv docs/users/migration/rename-constructors.md docs/reference/migration/rename-constructors.md

# Move API porting guide
git mv docs/users/api/pattern-construction.md docs/reference/features/pattern-construction.md

# Verify moves succeeded
ls docs/reference/migration/rename-constructors.md
ls docs/reference/features/pattern-construction.md
```

**Expected Result**: Both files are now in `docs/reference/` subdirectories

### Phase 4: Delete Redundant Directories

**Goal**: Remove inferior duplicates and empty directories

```bash
# Delete redundant guides directory
git rm -r docs/users/guides/

# Delete redundant examples directory
git rm -r docs/users/examples/

# Delete empty api directory
git rm -r docs/users/api/

# Delete empty migration directory
git rm -r docs/users/migration/

# Delete empty users directory
git rm -r docs/users/
```

**Expected Result**: All redundant directories removed

### Phase 5: Delete Redundant README Files

**Goal**: Remove unnecessary README files from subdirectories

```bash
# Delete README files (they should already be moved/deleted with directories above)
# But verify they're gone:
test ! -f docs/users/guide/README.md && echo "README.md deleted" || echo "Still exists"
test ! -f docs/users/README.md && echo "README.md deleted" || echo "Still exists"
test ! -f docs/reference/README.md && echo "README.md deleted" || echo "Still exists"
test ! -f docs/design/README.md && echo "README.md deleted" || echo "Still exists"
```

**Expected Result**: All redundant README files removed

### Phase 6: Update Root README.md

**Goal**: Update documentation link to point to new guide location

```bash
# Edit README.md (root)
# Change: docs/users/README.md → docs/guide/01-introduction.md
```

**Manual Step**: Update `README.md` (root) line 42:
- **Before**: `- **[User Documentation](docs/users/README.md)**`
- **After**: `- **[User Documentation](docs/guide/01-introduction.md)**`

### Phase 7: Update Guide File Links

**Goal**: Update internal links in guide files if needed

```bash
# Check for links that need updating
grep -r "docs/users" docs/guide/
grep -r "\.\./users" docs/guide/
```

**Manual Step**: Review guide files for links that reference old paths:
- Links within same directory (e.g., `[Introduction](01-introduction.md)`) remain valid
- Links to reference docs may need path updates
- Links to deleted README files must be removed

### Phase 8: Update Reference Documentation Links

**Goal**: Update links in reference docs to reflect new structure

```bash
# Check PORTING-GUIDE.md for user doc links
grep -n "docs/users" docs/reference/PORTING-GUIDE.md

# Check ARCHITECTURE.md for user doc links
grep -n "docs/users" docs/reference/ARCHITECTURE.md

# Check moved files for old path references
grep -n "docs/users" docs/reference/migration/rename-constructors.md
grep -n "docs/users" docs/reference/features/pattern-construction.md
```

**Manual Steps**:
1. Update `docs/reference/PORTING-GUIDE.md` line 40:
   - **Before**: `If you're a Haskell developer using the library, see **[User Documentation](../users/README.md)** instead.`
   - **After**: `If you're a Haskell developer using the library, see **[User Documentation](../guide/01-introduction.md)** instead.`

2. Update `docs/reference/ARCHITECTURE.md` if it has user doc links

3. Update `docs/reference/migration/rename-constructors.md` if it references moved files

4. Update `docs/reference/features/pattern-construction.md` if it references moved files

### Phase 9: Verification

**Goal**: Verify reorganization is complete and correct

```bash
# Verify guide files exist at new location
ls docs/guide/*.md | wc -l
# Should output: 8

# Verify reference files moved
test -f docs/reference/migration/rename-constructors.md && echo "✓ Migration guide moved"
test -f docs/reference/features/pattern-construction.md && echo "✓ API guide moved"

# Verify redundant directories deleted
test ! -d docs/users/guides && echo "✓ Redundant guides deleted"
test ! -d docs/users/examples && echo "✓ Redundant examples deleted"
test ! -d docs/users && echo "✓ Users directory deleted"

# Verify redundant README files deleted
test ! -f docs/reference/README.md && echo "✓ Reference README deleted"
test ! -f docs/design/README.md && echo "✓ Design README deleted"

# Check for broken links (manual review needed)
grep -r "docs/users" docs/guide/ docs/reference/ README.md
# Should find no references to old paths (except in historical context)
```

**Expected Result**: All verification checks pass

## Common Issues

### Issue: `git mv` fails because target directory doesn't exist

**Solution**: Create target directory first:
```bash
mkdir -p docs/reference/migration
git mv docs/users/migration/rename-constructors.md docs/reference/migration/
```

### Issue: Links still reference old paths

**Solution**: Use grep to find all references:
```bash
grep -r "docs/users" docs/
grep -r "\.\./users" docs/
```
Then update each file manually.

### Issue: Git shows files as deleted and added instead of moved

**Solution**: This is normal for some git operations. The important thing is that `git mv` was used, which preserves history. Git will track the rename automatically.

## Success Criteria

✅ All guide files accessible at `docs/guide/`  
✅ All reference files consolidated in `docs/reference/`  
✅ All redundant directories deleted  
✅ All redundant README files deleted  
✅ Root README.md points to new guide location  
✅ All internal links resolve correctly  
✅ No broken links remain  

## Next Steps

After completing this quickstart:
1. Review all link updates manually
2. Test documentation navigation
3. Commit changes with descriptive commit message
4. Verify git history preserved (check `git log --follow`)

