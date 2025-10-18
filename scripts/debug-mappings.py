#!/usr/bin/env python3
"""Debug script to check mappings."""

import sys
sys.path.insert(0, 'scripts')
from process_monitor_calls import MonitorCallsProcessor

p = MonitorCallsProcessor(
    'Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls-WEB.md', 
    'test.md'
)
p.read_file()
p.parse_section3()

print('\nChecking AccessRTCommon...')
print(f"'ACCESSRTCOMMON' in long_name_to_anchor: {'ACCESSRTCOMMON' in p.long_name_to_anchor}")
print(f"Value: {p.long_name_to_anchor.get('ACCESSRTCOMMON', 'NOT FOUND')}")

print('\nAll keys starting with ACCESS:')
for k in sorted(p.long_name_to_anchor.keys()):
    if 'ACCESS' in k.upper():
        print(f"  {k} -> {p.long_name_to_anchor[k]}")

print(f'\nTotal long_name mappings: {len(p.long_name_to_anchor)}')
print(f'Total short_name mappings: {len(p.call_map)}')

