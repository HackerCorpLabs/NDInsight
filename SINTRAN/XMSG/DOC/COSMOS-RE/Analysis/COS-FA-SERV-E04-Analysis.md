# cos-fa-serv-e04.prog — Reverse-Engineering Analysis (COSMOS File-Access Server)

**Program:** `Installation/Communication/COSMOS Basic/x/cos-fa-serv-e04.prog`
**Identity:** COSMOS **File-Access Server** (`*FA-SERVER` port 11, `*FA-FSA` port 7). ND-100 SINTRAN-III `:PROG`, **PLANC**, banked (BANK2), 329 functions.
**RT program:** launched by `cos-fa-serv-e04.mode` (`START-TADADM`, `FS-ADMINISTRATOR → SELECT-FSA, START-SERVER 1`).
**Status tags:** `[BIN]` decoded from binary in Ghidra · `[SYM]` ND symbol files · `[DOC]` XMSG/TAD specs · `[INF]` inferred.

> Instruction semantics checked against `E:\Dev\Ronny\nd100-markdown\docs\cpu_documentation.md`.
> ~179 functions named + commented in the Ghidra DB (all protocol-significant logic).

---

## 1. Role & startup `[BIN]`

The server registers **two service names with XROUT** and then loops receiving requests:

1. **`fa_init_global_registries`** (0x3fee) + **`fa_init_server_data_structures`** (0x2d68): build the file-entry table, the 8-word-block free-lists, and the global registries `g_fa_reg_904c/904d/904e/904f`; seed config (block size `g_fa_blocksize_9020` = 100).
2. **`fa_server_main`** (0x0500): build an XROUT letter (`byte[1]=0x45` = service request, bit 6 set = XSGNI-class), send it **routed + waited** (XFSND options `0x400`=XFROU, `0x8000`=XFWTF) to register `*FA-SERVER`(11)/`*FA-FSA`(7); then `XFOPN` the server port and `XFRCV` the first request.
3. **`cos_fa_serv_e04`** (0x23c3, program entry): init globals, run the request loop.
4. **`fa_recv_request_wait`** (0x8c5d): `XFRCV` (option XFWTF) → read the request; **`fa_request_engine_process`** (0x8c99) is the core engine (walks file-entry lists by session sequence, allocates/initialises the ~0x2e-word entry descriptor, drives the reply).

**Transport:** identical XMSG `MON 200B` API as the other COSMOS programs — one wrapper per XF function (`xmsg_XFOPN/XFCLS/XFGET/XFREL/XFGST/XFREA/XFWRI/XFSND/XFRCV/XFMST/XFSCM`, ram:a0bc–a1e7). Send = `XFGET→XFWRI→XFSND`; receive = `XFRCV→XFREA→XFREL`.

---

## 2. The file-access request protocol `[BIN]`

### 2.1 Request operations (the 13 ops)

Command-name table at `BANK2::8731`. Each op is decoded from the request and handled:

| Op | Handler | What it does |
|----|---------|--------------|
| Reserve-file-entry | `fa_reserve_file_entry` (2ca5) | find entry, set reservation bits `entry[+0xa]` |
| Release-file-entry | `fa_release_file_entry_op` (34cd) | type 2 → clear reservation bits |
| Change-file-entry-id | `fa_change_file_entry_id` (2e12) | type 8 → accumulate/replace `entry[+0xb]` id/attrs |
| Open-file | `fa_open_file_op` (2eae) | check lock bit + size, build access reply |
| Close-file | `fa_close_file_decrement_ref` (2f2d) | decrement refcount, free at 0 |
| Read-file / Write-file | `fa_file_data_transfer` (315b) | type 0x10 → position/count in entry data buffer (~0x800 B) |
| Create-file | `fa_create_file_entry` (3294) + `fa_process_named_file_entry` (3332) | validate limits, set reservation + name |
| Delete-file | `fa_delete_file_entry_op` (34f8) | type 1 → release all + reinit |
| Set-block-size | `fa_blocksize_config_op` (33d6) | return/negotiate `g_fa_blocksize_9020` |
| File-entry-disconnect | `fa_release_all_session_entries` (27f4) | free all entries for the session |
| SIII-special / Device-function | via the param dispatchers | (routed through `fa_process_params_dispatch`) |

**Entry type** (top-2-bits of `entry[+1]`): 1=deletable, 2=reserved, 8=change/data, 0x10=data-transfer, 0x80=named.
**Status/error codes:** 0=ok, 0x28=reserve, 0x29=release, 3=bad-type, 5=table-full, 8=already-free, 0xd=not-reserved.

### 2.2 The file-entry descriptor (~0x2e words) `[BIN][INF]`

```
[+0]      packed flags / link
[+1]      type (top-2-bits) + class
[+0xa]    reservation / owner-lock bits (SHA-rotated sub-field)
[+0xb..]  id / attributes / size / position counters
[+5],[+6] per-session sub-chains
[+8]      id / name handle
[+0x11]   result state (2=ok, 3/4=error)
[+~0x7ba] embedded data buffer (~0x800-byte page): position/count fields
[+~0x7bf] far status word (bit15 = valid/lock)
```
Entries live in bitmap-allocated slots (`fa_bitmap_find_free_slot` 26d9 / `fa_bitmap_free_slot` 271a) and are chained in the global registries `g_fa_reg_904c/904d/904e/904f`.

### 2.3 The wire body: QFORM typed-parameter list `[BIN]`

Both request and reply bodies are a list of **typed parameters** `[tag][value]` (same model as `cos-xftra`/`cos-file-tra`):
- **Parse (request):** `fa_parse_request_params` (29c0) → per-field `fa_process_params_dispatch` (35da, table `g_fa_param_dispatch_table` 9039) / `_v2` (3b34, table 9044). Tags: `0x01`, `0x10`, `0x80`=STRING.
- **Emit (reply):** the emitter primitives write a **1-byte tag then the value**. The tag bytes are
  read directly from each emitter's tag word `[BIN-VERIFIED]`:

  | Emitter | Addr | Tag word | **Tag byte** | Value length |
  |---------|------|----------|-------------|--------------|
  | `msg_put_param_word`   | 7a55 | @0x7a8c | **`0x92`** | 2 bytes (INT16) |
  | `msg_put_param_dword`  | 7a91 | @0x7ac8 | **`0x94`** | 4 bytes (INT32) |
  | `msg_put_param_typed_b`| 7acd | @0x7b04 | **`0xA2`** | 2 bytes (class A) |
  | `msg_put_param_typed_c`| 7b45 | @0x7b7c | **`0xF2`** | 2 bytes (class F — string/name?) |

  Tag encoding appears to be `(type_class << 4) | length_in_bytes` `[INFERRED]` (class 9=integer,
  A/F = other classes; the exact class→type meaning needs the param-reader dispatch decode).
  Driven by the reply serializers `fa_build_entry_reply_fields` (36d1) / `_v3..v5` /
  `fa_build_full_entry_reply` (393a) / `fa_build_typed_reply_dispatch` (3808, table
  `g_fa_reply_emit_table` 903d).

  **CAVEAT — request-parse tags unverified:** `fa_parse_request_params` (0x29c0) compares the field
  tag against `0x01/0x10/0x80`. Those are the decoded *internal* type indices; whether they equal the
  raw wire tag byte (vs the emit-side 0x92/0x94/0xA2/0xF2) has NOT been byte-traced — tag as
  `[UNVERIFIED]` until the parser's tag-load instruction is read.

---

## 3. Support layers `[BIN]`

- **SINTRAN OS-call library** (ram:0x77f2–0x796e, 0xa6xx): file monitor calls — `mon_open_file` (OPEN 50), `mon_close_file` (CLOSE 43), `mon_create_file` (CRALF 221), `mon_direct_open` (DOPEN 220), `mon_expand_file` (EXPFI 231), `mon_get_dir_entry` (GDIEN 244), `mon_get_all_file_indexes` (GUIOI 217), `mon_new_file_version` (CRALN 253), `mon_get_file_size` (RMAX 62), `mon_get_user_name/entry`, `mon_new/old_user` (SUSCN/RUSCN), `mon_set_command_buffer` (SETCM), `mon_get_time` (TIME), `mon_exit_program` (LEAVE), etc.
- **Error/panic handlers** (0xa831–0xa894): `err_handler_walk_stack`, `err_print_routine_name`, `err_print_string_and_hexnum`, `err_handler_stack_overflow` — print `"NO ROUTINE ERROR HANDLER, ERRETURN='"` / `"STACK OVERFLOW AT '"` via OUTBT/QERMS.
- **Segment/overlay copy** (`fa_altpage_segment_copy_open` a792): ALTON/ALTOFF page-table mapping to copy between segments + OPEN with block size.
- **Operator status message** (`fa_format_server_status_msg` 2645): `"FA-server NN active/unavail/terminated"` (BANK2::842c).

---

## 4. Architecture notes `[BIN]`

- **Second subsystem copy:** `0x42xx–0x4e6f` is a duplicate of the `0x26xx–0x3fxx` entry-management logic (validate/alloc/free/serialize), operating on the secondary registry `g_fa_reg_904c` instead of the main entry table (`_v2` suffixed functions).
- **Ghidra fragment inflation:** many `FUN_ram_*` in the dense/garbled regions are mid-body fragments of large routines (e.g. `frag_of_fa_request_engine_8cd4`), not distinct functions — the real distinct-function count is well below 329.
- **Dispatch:** `fa_dispatch_by_type3bits` (08b1) computed-jumps on a 3-bit type via `BANK2::8477`; the per-op parameter and reply dispatchers use tag-indexed jump tables (9039/9044/903d/9046).

---

## 5. Open items

- The literal renaming of the `_v2` duplicate subsystem + BANK2 utility helpers + Ghidra fragments (mechanical; no new semantics).
- Exact per-op parameter schemas (which typed params each op carries) — inferable from the dispatch tables, would be confirmed by a capture of a live FA session.

---

## 6. Function-naming completeness (deliberately left)

**Status:** all *protocol-significant* functions are named + commented (~190). The remaining
`FUN_ram_*` are **intentionally left** — they add names, not understanding — and are documented here
so the region is mechanically completable later if desired.

### What remains, and why it's left
| Region | ~count | What it is | Why left |
|--------|-------:|-----------|----------|
| `0x42xx–0x4e6f` | ~34 | **`_v2` duplicate** of the `0x26xx–0x3fxx` entry/registry subsystem, operating on the secondary registry `g_fa_reg_904c` | byte-for-byte copies of already-decoded functions |
| BANK2 `0x82xx+` | some | utility helpers | non-protocol |
| dense/garbled areas | many | **Ghidra fragment inflation** — phantom splits of large routines (e.g. `frag_of_fa_request_engine_8cd4`) | not real functions |

### Mechanical completion recipe (`_v2` region)
Each `0x4xxx` function is `<0x2xxx-original>_reg2`. Correlate by structure and name accordingly.
Already named this way: `fa_bitmap_free_slot_reg2` (0x4391), `fa_init_entry_block_reg2` (0x43dd),
`fa_build_reply_from_list_reg2` (0x43eb), `fa_release_entry_slot18/19/1a/1b_reg2`
(0x4421/0x4447/0x446d/0x4493 — a run of release-entry-slot-N helpers, one per slot offset),
`fa_registry_helper_4056`, plus the earlier `fa_clear_reg_904c` (0x42ff),
`fa_validate_index_build_reply_v2` (0x4310), `fa_bitmap_free_slot_v2` (0x4350).

**To verify a `FUN_ram_*` is a fragment (skip it), not a real function:** check whether its decompile
is a byte-identical tail of a neighbouring routine. If so, it's a phantom split — do not name it as
independent logic.
