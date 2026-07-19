# Code logic

Reverse-engineering of the `006-S3FS` filesystem code: directory/user/file
allocation, the bitmap get-page/release-page primitives, file I/O
(read/write/append/delete), and CRDIR (create-directory / boot-sector creation).
Covers Phases 5-7 of the plan.

See the [Filesystem foundation](../README.md) for the `006-S3FS` code map
(FILSYS symbol addresses + roles) and the open questions each phase must resolve.

## Documents (Phases 5-6)

- [`s3fs-code-map.md`](s3fs-code-map.md) - routine map of the `006-S3FS`
  filesystem code, grouped by area (open/close, read/write, create/delete/
  rename/expand, object/user/directory access, bitmap primitives, index blocks),
  each row anchored to a carved MON worker. Explains the two structural idioms
  (SSK/SSM flag fork; PLANC frame + indirect call table).
- [`allocation.md`](allocation.md) - the page-bitmap allocation logic: the
  one-bit primitives (`ALPAG`/`RLPAG`/`TPAGF`), the free-page scanner
  (`TESTP`), the get-page allocator (`GPAGE`), the create-alloc dispatcher
  (`CRALN`/`CRALF`/`EXPFI`/`SFACC`), object-entry creation (`CROBJ`) and the
  quota hook (`CUSED`).
  **Resolves the search-direction open question: allocation scans HIGH -> LOW
  (downward from the top of the bitmap to the block-7 floor)** - matching the
  `@CREATE-FILE` "highest range" rule and refuting the NDFS reader's naive
  upward search. Byte proof: `AAX -1` in `TESTP` (51372B / 51401B).
- [`enter-directory.md`](enter-directory.md) - the full end-to-end
  `@ENTER-DIRECTORY` (mount) trace: command dispatch -> `ENDIR` 140176B (unit
  reserve via `MON 124` + `GDIRA` datafield) -> `CHDSI` 37763B -> the **page-0
  read** (`RXDIR` 37643B / `RCBLO` 35766B, logical block 0, dispatched through the
  device datafield transfer pointer `,X 14`) -> additive checksum, capacity
  compare, owner interlock, flag bit-15 stamp -> `WXDIR` 37702B write-back. Includes
  the exact disk-controller read contract (block 0, 2048-byte page, SCSI-geometry
  implication) and the mount error-code table.
- [`file-io.md`](file-io.md) - read/write/append/delete flow from the open-file
  handle through the object-entry page list to the disk-page primitives
  (`RDPAG`/`WDPAG`) and the bitmap; the block->physical addressing (page = 1024
  words = 2048 bytes, `SHA ZIN 12`); indexed vs contiguous page lookup; the
  delete/rename dispatcher (`MDLFI`).

**Get-page primitive:** `GPAGE` 76205B (routes through `RSPAG` 51120B +
`CUSED` 55206B). **Free-page primitive:** `RLPAG` 50635B.
