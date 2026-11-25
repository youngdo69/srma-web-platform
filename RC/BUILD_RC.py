"""Build and verify the exact 6.3.16 working-tree snapshot with Python 3.12+."""

import argparse
import hashlib
import json
import stat
from pathlib import Path, PurePosixPath
from zipfile import ZIP_DEFLATED, ZipFile, ZipInfo


RC_ID = "srma-6.3.16-rc1"
TIMESTAMP = (2026, 11, 24, 0, 0, 0)
SOURCE_MANIFEST = "SOURCE_MANIFEST.json"
DOCS = (
    SOURCE_MANIFEST,
    "BUILD_RC.py",
    "INSTALL_AND_RUN.md",
    "MANUSCRIPT_PUBLICATION_QC.md",
    "RC_VALIDATION_MANIFEST.json",
    "RELEASE_NOTES.md",
    "UNIVERSAL_FUNCTION_ACCEPTANCE.md",
)


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def source_path(root, name):
    relative = PurePosixPath(name)
    if (relative.is_absolute() or relative.as_posix() != name
            or not relative.parts or ".." in relative.parts or ":" in name
            or "\\" in name or any(part.rstrip(" .") != part for part in relative.parts)):
        raise ValueError(f"Invalid member path: {name}")
    current = root
    for part in relative.parts:
        current /= part
        info = current.lstat()
        if stat.S_ISLNK(info.st_mode) or (
            getattr(info, "st_file_attributes", 0) & stat.FILE_ATTRIBUTE_REPARSE_POINT
        ):
            raise ValueError(f"Linked or redirected source: {name}")
    if not current.is_file():
        raise ValueError(f"Expected file: {name}")
    return current


def read_inputs(root, docs, source_only=False):
    manifest = json.loads(source_path(docs, SOURCE_MANIFEST).read_bytes())
    if manifest["rc_id"] != RC_ID:
        raise ValueError("Wrong source manifest for this builder")
    members = {}
    for name, expected in manifest["files"].items():
        data = source_path(root, name).read_bytes()
        if sha256(data) != expected:
            raise ValueError(f"Source SHA-256 mismatch: {name}")
        members[name] = data
    if not source_only:
        for name in DOCS:
            members[f"RC/{name}"] = source_path(docs, name).read_bytes()
    if len({name.casefold() for name in members}) != len(members):
        raise ValueError("Case-insensitive path collision")
    return members


def check_archive(archive, expected):
    with ZipFile(archive) as zipped:
        if zipped.namelist() != sorted(expected) or zipped.testzip() is not None:
            raise ValueError("Archive inventory or CRC check failed")
        for name, data in expected.items():
            if sha256(zipped.read(name)) != sha256(data):
                raise ValueError(f"Archive SHA-256 mismatch: {name}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path, help="Root of the exact source snapshot")
    parser.add_argument("archive", type=Path, help="New or existing ZIP file")
    action = parser.add_mutually_exclusive_group()
    action.add_argument("--verify", action="store_true", help="Verify an existing archive")
    action.add_argument("--source-only", action="store_true", help="Build just the runtime payload for isolated preflight")
    args = parser.parse_args()
    docs = Path(__file__).resolve().parent
    members = read_inputs(args.source.resolve(strict=True), docs, args.source_only)
    if not args.verify:
        with ZipFile(args.archive, "x", compression=ZIP_DEFLATED) as zipped:
            for name in sorted(members):
                info = ZipInfo(name, TIMESTAMP)
                info.create_system = 3
                info.external_attr = (stat.S_IFREG | 0o644) << 16
                zipped.writestr(info, members[name], compress_type=ZIP_DEFLATED, compresslevel=9)
    check_archive(args.archive, members)
    print(f"PASS: {len(members)} archive members verified")
    print(f"SHA-256 {sha256(args.archive.read_bytes())}")


if __name__ == "__main__":
    main()
