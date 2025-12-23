# Implementation Summary: Decouple Identity Assignment

**Feature**: 025-decouple-identity-assignment  
**Date**: 2025-01-XX  
**Status**: Complete

## Overview

This implementation decouples identity assignment from the transformation phase, making it optional and preserving anonymity by default for round-trip compatibility.

## What Changed

### Core Implementation

1. **`Gram.Transform.transformGram`** (Default behavior)
   - Now preserves anonymous subjects as `Symbol ""` instead of generating IDs
   - Removed state initialization (no longer needs counter)
   - Updated documentation to note anonymity preservation

2. **`Gram.Transform.transformGramWithIds`** (New function)
   - Explicit ID assignment variant
   - Equivalent to `assignIdentities . transformGram`
   - Assigns sequential IDs (`#1`, `#2`, etc.) to anonymous subjects

3. **`Gram.Transform.assignIdentities`** (New function)
   - Post-transform ID assignment
   - Recursively assigns IDs to subjects with `Symbol ""`
   - Starts counter from max existing `#N`-style ID + 1 to avoid collisions

4. **`Gram.Transform.findMaxIdInPattern`** (New helper)
   - Extracts maximum numeric suffix from `#N`-style IDs in a Pattern
   - Used by `assignIdentities` to determine starting counter value

5. **`Gram.Parse.fromGram`** (Updated)
   - Documentation updated to note it preserves anonymous subjects
   - Behavior unchanged (uses `transformGram` which now preserves anonymity)

6. **`Gram.Parse.fromGramWithIds`** (New function)
   - Explicit ID assignment variant
   - Uses `transformGramWithIds` internally

### Test Updates

1. **ParseSpec Updates**
   - Updated tests expecting IDs to use `fromGramWithIds`
   - Updated general parsing tests to check for `Symbol ""` instead of generated IDs
   - Added comprehensive anonymous preservation tests
   - Added `assignIdentities` tests

2. **SerializeSpec Updates**
   - Added round-trip tests for anonymous subjects
   - Updated property test to handle anonymous subjects (structural equality)
   - Added test for `assignIdentities` round-trip behavior

3. **CorpusSpec Updates**
   - Added test for anonymous subject round-trip in corpus examples
   - Verified existing round-trip tests use structural equality (works correctly)

4. **Integration Tests**
   - Added end-to-end anonymous preservation workflow test
   - Added mixed workflow test (fromGram vs fromGramWithIds)

## What Stayed the Same

- **Validation**: Unaffected - operates on CST before transformation
- **Serialization**: Already supports `Symbol ""` as anonymous
- **Core Pattern/Subject types**: No changes
- **API compatibility**: `fromGram` signature unchanged (behavior improved)

## Migration Notes

### For External Code

- **Default behavior change**: `fromGram` now preserves anonymity instead of assigning IDs
- **If you need IDs**: Use `fromGramWithIds` or `assignIdentities`
- **Round-trip compatibility**: Now works correctly for anonymous subjects

### Code Patterns

**Before:**
```haskell
-- Anonymous subjects got IDs like #1, #2
let Right p = fromGram "() ()"
-- p has subjects with Symbol "#1" and Symbol "#2"
```

**After (default):**
```haskell
-- Anonymous subjects preserved as Symbol ""
let Right p = fromGram "() ()"
-- p has subjects with Symbol "" and Symbol ""
```

**After (with IDs):**
```haskell
-- Explicit ID assignment when needed
let Right p = fromGramWithIds "() ()"
-- p has subjects with Symbol "#1" and Symbol "#2"
```

## Benefits

1. **Round-trip compatibility**: `() -> parse -> serialize -> parse -> ()` now works
2. **Flexibility**: Choose between anonymity preservation and ID assignment
3. **Validation unaffected**: Still operates on CST before transformation
4. **Backward compatible API**: Same function signatures, improved behavior

## Testing

- All existing tests updated appropriately
- Comprehensive new tests for anonymous preservation
- Round-trip tests verify structural equality
- Integration tests verify both workflows
- Corpus tests verified

## Files Modified

- `libs/gram/src/Gram/Transform.hs` - Core transformation logic
- `libs/gram/src/Gram/Parse.hs` - Parse functions
- `libs/gram/tests/Spec/Gram/ParseSpec.hs` - Parse tests
- `libs/gram/tests/Spec/Gram/SerializeSpec.hs` - Serialize tests
- `libs/gram/tests/Spec/Gram/CorpusSpec.hs` - Corpus tests

## Verification

- ✅ Code compiles without errors
- ✅ All linter checks pass
- ✅ Tests updated and comprehensive
- ✅ Documentation updated
- ✅ Validation module unaffected
- ✅ Round-trip compatibility verified

