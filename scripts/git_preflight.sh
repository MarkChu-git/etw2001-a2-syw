#!/usr/bin/env bash
set -u

TARGET_DIR="${1:-.}"

json_escape() {
  python3 -c 'import json,sys; print(json.dumps(sys.stdin.read())[1:-1])'
}

emit_json() {
  local status="$1"
  local summary="$2"
  local repo_root="${3:-}"
  local branch="${4:-}"
  local dirty="${5:-}"
  local next_actions="${6:-}"
  cat <<JSON
{
  "status": "$status",
  "summary": "$summary",
  "repo_root": "$repo_root",
  "branch": "$branch",
  "dirty": $dirty,
  "next_actions": "$next_actions"
}
JSON
}

if ! command -v git >/dev/null 2>&1; then
  emit_json "error" "Git is not installed or not on PATH." "" "" "[]" "Install Git before editing. macOS: xcode-select --install or brew install git; Ubuntu/Debian: sudo apt-get update && sudo apt-get install -y git; Fedora: sudo dnf install -y git; Windows: winget install --id Git.Git -e."
  exit 2
fi

if ! cd "$TARGET_DIR" >/dev/null 2>&1; then
  emit_json "error" "Target directory cannot be opened." "" "" "[]" "Check the path and rerun preflight."
  exit 2
fi

if ! REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)"; then
  emit_json "warning" "Directory is not a Git repository." "" "" "[]" "Run git init from the assignment/project root before editing."
  exit 1
fi

BRANCH="$(git branch --show-current 2>/dev/null || true)"
STATUS="$(git status --short 2>/dev/null || true)"
DIRTY_JSON="$(printf '%s\n' "$STATUS" | python3 -c 'import json,sys; lines=[l for l in sys.stdin.read().splitlines() if l.strip()]; print(json.dumps(lines))')"

if [ -n "$STATUS" ]; then
  emit_json "warning" "Git repository found with dirty worktree." "$REPO_ROOT" "$BRANCH" "$DIRTY_JSON" "List dirty files, separate user changes from planned changes, and avoid staging unrelated files."
  exit 1
fi

emit_json "success" "Git repository found with clean worktree." "$REPO_ROOT" "$BRANCH" "[]" "Proceed with assignment workflow."
