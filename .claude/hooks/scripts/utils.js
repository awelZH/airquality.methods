/**
 * Cross-platform utilities for Claude Code hooks
 * Compatible with Windows, macOS, and Linux
 */

const fs = require('fs');
const path = require('path');
const os = require('os');

/**
 * Get the Claude data directory (cross-platform)
 */
function getClaudeDir() {
  const home = os.homedir();
  return path.join(home, '.claude');
}

/**
 * Get the sessions directory for storing session state
 */
function getSessionsDir() {
  return path.join(getClaudeDir(), 'sessions');
}

/**
 * Get the learned skills directory
 */
function getLearnedSkillsDir() {
  return path.join(getClaudeDir(), 'learned');
}

/**
 * Get temp directory (cross-platform)
 */
function getTempDir() {
  return os.tmpdir();
}

/**
 * Ensure a directory exists
 */
function ensureDir(dirPath) {
  if (!fs.existsSync(dirPath)) {
    fs.mkdirSync(dirPath, { recursive: true });
  }
}

/**
 * Read file contents, returns null if file doesn't exist
 */
function readFile(filePath) {
  try {
    return fs.readFileSync(filePath, 'utf8');
  } catch (e) {
    return null;
  }
}

/**
 * Write content to file
 */
function writeFile(filePath, content) {
  ensureDir(path.dirname(filePath));
  fs.writeFileSync(filePath, content, 'utf8');
}

/**
 * Append content to file
 */
function appendFile(filePath, content) {
  ensureDir(path.dirname(filePath));
  fs.appendFileSync(filePath, content, 'utf8');
}

/**
 * Get current date-time string (YYYY-MM-DD HH:MM:SS)
 */
function getDateTimeString() {
  const now = new Date();
  return now.toISOString().replace('T', ' ').substring(0, 19);
}

/**
 * Get current date string (YYYY-MM-DD)
 */
function getDateString() {
  return new Date().toISOString().substring(0, 10);
}

/**
 * Get current time string (HH:MM:SS)
 */
function getTimeString() {
  return new Date().toISOString().substring(11, 19);
}

/**
 * Generate short ID for session files
 */
function shortId() {
  return Math.random().toString(36).substring(2, 8);
}

/**
 * Find files matching a pattern in a directory
 * @param {string} dir - Directory to search
 * @param {string} pattern - Glob-like pattern (supports * wildcard)
 * @param {object} options - { maxAge: days to look back }
 */
function findFiles(dir, pattern, options = {}) {
  if (!fs.existsSync(dir)) {
    return [];
  }

  const files = fs.readdirSync(dir);
  const regex = new RegExp('^' + pattern.replace(/\*/g, '.*') + '$');
  const maxAge = options.maxAge ? options.maxAge * 24 * 60 * 60 * 1000 : Infinity;
  const now = Date.now();

  return files
    .filter(f => regex.test(f))
    .map(f => {
      const fullPath = path.join(dir, f);
      const stat = fs.statSync(fullPath);
      return {
        name: f,
        path: fullPath,
        mtime: stat.mtime,
        age: now - stat.mtime.getTime()
      };
    })
    .filter(f => f.age <= maxAge)
    .sort((a, b) => b.mtime - a.mtime); // Most recent first
}

/**
 * Log message to stderr (visible in hook output)
 */
function log(message) {
  console.error(message);
}

/**
 * Check if a hook is disabled.
 *
 * Checks in priority order:
 *  1. CLAUDE_DISABLE_ALL_HOOKS=1  — disables every hook
 *  2. CLAUDE_DISABLE_HOOKS=a,b,c  — disables named hooks for this process
 *  3. .claude/hooks/state.json    — persistent per-project disable list
 *
 * To disable a hook for the current session only (no file edits needed):
 *   export CLAUDE_DISABLE_HOOKS=doc-blocker
 *   export CLAUDE_DISABLE_HOOKS=doc-blocker,git-push-warn
 *   export CLAUDE_DISABLE_ALL_HOOKS=1
 *
 * To disable persistently for a project, edit .claude/hooks/state.json:
 *   { "disabled": ["doc-blocker"] }
 *
 * Available hook names:
 *   doc-blocker, git-push-warn, suggest-compact,
 *   session-start, session-end, pre-compact
 *
 * @param {string} hookName - Hook name (e.g. 'git-push-warn', 'doc-blocker')
 * @returns {boolean}
 */
function isHookDisabled(hookName) {
  // 1. Kill-switch: disable every hook
  if (process.env.CLAUDE_DISABLE_ALL_HOOKS === '1') return true;

  // 2. Env-var list: CLAUDE_DISABLE_HOOKS=doc-blocker,git-push-warn
  const envList = process.env.CLAUDE_DISABLE_HOOKS;
  if (envList) {
    const names = envList.split(',').map(s => s.trim());
    if (names.includes(hookName)) return true;
  }

  // 3. Persistent state file: .claude/hooks/state.json
  // Use __dirname (script location) rather than process.cwd() since Claude Code
  // may run hooks from a different working directory than the project root.
  // Scripts live at .claude/hooks/scripts/, state.json is at .claude/hooks/state.json
  const stateFile = path.join(__dirname, '..', 'state.json');
  try {
    const state = JSON.parse(fs.readFileSync(stateFile, 'utf8'));
    return Array.isArray(state.disabled) && state.disabled.includes(hookName);
  } catch (e) {
    return false;
  }
}

module.exports = {
  getClaudeDir,
  getSessionsDir,
  getLearnedSkillsDir,
  getTempDir,
  ensureDir,
  readFile,
  writeFile,
  appendFile,
  getDateTimeString,
  getDateString,
  getTimeString,
  shortId,
  findFiles,
  log,
  isHookDisabled
};
