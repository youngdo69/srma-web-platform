#!/usr/bin/env python3

from pathlib import Path
import platform
import sys

# 이 파일이 있는 scripts 폴더의 한 단계 위가 프로젝트 루트입니다.
PROJECT_ROOT = Path(__file__).resolve().parent.parent

REQUIRED_FOLDERS = [
    "skills",
    "harnesses",
    "templates",
    "scripts",
]

REQUIRED_FILES = [
    "README.md",
    "AGENTS.md",
    "CLAUDE.md",
    ".gitignore",
    ".env.example",
]


def show_result(name: str, exists: bool) -> bool:
    if exists:
        print(f"[OK]   {name}")
    else:
        print(f"[FAIL] {name}")
    return exists


def main() -> None:
    print("=" * 55)
    print("SRMA Agent Toolkit Installation Check")
    print("=" * 55)

    print(f"\nProject folder: {PROJECT_ROOT}")
    print(f"Python version: {platform.python_version()}")

    all_ok = True

    print("\n1. Checking Python version")

    python_ok = sys.version_info >= (3, 10)
    all_ok = show_result("Python 3.10 or newer", python_ok) and all_ok

    print("\n2. Checking folders")

    for folder_name in REQUIRED_FOLDERS:
        folder_path = PROJECT_ROOT / folder_name
        result = show_result(folder_name, folder_path.is_dir())
        all_ok = result and all_ok

    print("\n3. Checking files")

    for file_name in REQUIRED_FILES:
        file_path = PROJECT_ROOT / file_name
        result = show_result(file_name, file_path.is_file())
        all_ok = result and all_ok

    print("\n4. Checking local .env file")

    env_path = PROJECT_ROOT / ".env"

    if env_path.is_file():
        print("[OK]   .env")
    else:
        print("[INFO] .env is not present yet.")
        print("       This is acceptable until API keys are needed.")

    print("\n" + "=" * 55)

    if all_ok:
        print("SUCCESS: The basic project structure is ready.")
    else:
        print("ATTENTION: One or more required items are missing.")

    print("=" * 55)


if __name__ == "__main__":
    main()