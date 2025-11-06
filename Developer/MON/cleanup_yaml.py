#!/usr/bin/env python3
"""
Clean up extracted YAML files:
- Remove code fences (```) from examples
- Clean up parameter names
- Fix any formatting issues
"""

import yaml
from pathlib import Path
import re


def clean_code_example(code: str) -> str:
    """Remove code fences and clean up code examples"""
    if not code:
        return code

    # Remove code fences
    code = code.replace('```', '')

    # Strip leading/trailing whitespace but preserve internal formatting
    lines = code.split('\n')

    # Remove leading/trailing empty lines
    while lines and not lines[0].strip():
        lines.pop(0)
    while lines and not lines[-1].strip():
        lines.pop()

    return '\n'.join(lines)


def clean_param_name(name: str) -> str:
    """Clean up parameter names"""
    # Remove arrow character
    name = name.replace('➙', '').strip()

    # If the name is actually a description, extract just the name
    # e.g., "File name. It may be abbreviated..." -> "FileName"
    if '. ' in name:
        name = name.split('.')[0].strip()

    # Remove spaces
    name = name.replace(' ', '')

    return name


def cleanup_yaml_file(filepath: Path):
    """Clean up a single YAML file"""
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # Separate header comments from YAML content
    header_lines = []
    yaml_start = 0
    for i, line in enumerate(lines):
        if line.startswith('#'):
            header_lines.append(line)
        else:
            yaml_start = i
            break

    # Load YAML content
    yaml_content = ''.join(lines[yaml_start:])
    data = yaml.safe_load(yaml_content)

    # Clean up examples
    if 'examples' in data:
        for lang, example in data['examples'].items():
            if 'code' in example and example['code']:
                example['code'] = clean_code_example(example['code'])

    # Clean up parameters
    if 'parameters' in data:
        for param in data['parameters']:
            if 'name' in param:
                original_name = param['name']
                param['name'] = clean_param_name(original_name)

                # If name was a description, move it to description
                if '. ' in original_name and not param.get('description'):
                    param['description'] = original_name.replace('➙', '').strip()

    # Write back
    with open(filepath, 'w', encoding='utf-8') as f:
        # Write header
        for line in header_lines:
            f.write(line)
        f.write('\n')

        # Write YAML
        yaml.dump(data, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

    return True


def main():
    """Process all YAML files"""
    calls_dir = Path('Developer/MON/calls')

    if not calls_dir.exists():
        print(f"❌ Directory not found: {calls_dir}")
        return

    yaml_files = list(calls_dir.glob('*.yaml'))
    print(f"🔧 Cleaning up {len(yaml_files)} YAML files...")

    success = 0
    failed = 0

    for filepath in sorted(yaml_files):
        try:
            cleanup_yaml_file(filepath)
            success += 1
            if success % 20 == 0:
                print(f"  ✓ Processed {success}/{len(yaml_files)}...")
        except Exception as e:
            print(f"  ✗ Failed: {filepath.name}: {e}")
            failed += 1

    print(f"\n✅ Cleanup complete!")
    print(f"  Success: {success}")
    print(f"  Failed: {failed}")


if __name__ == "__main__":
    main()
