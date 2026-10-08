#!/usr/bin/env bash

# Claude Code status line: model, effort, context usage, rate limits, git branch, session name.
# Reads the session JSON from stdin, see https://code.claude.com/docs/en/statusline

input=$(cat)

dir=$(jq -r '.workspace.current_dir // .cwd // empty' <<<"$input")
branch=$(git -C "${dir:-.}" branch --show-current 2>/dev/null)
# Detached HEAD has no branch name, fall back to the commit
[[ -z $branch ]] && branch=$(git -C "${dir:-.}" rev-parse --short HEAD 2>/dev/null)

jq -r --arg branch "$branch" '
  def reset: "\u001b[0m";
  def dim: "\u001b[2m" + . + reset;

  # Thresholds differ in brightness and weight, not only in hue
  def colored($used_percentage):
    (if $used_percentage >= 80 then "\u001b[1;31m" elif $used_percentage >= 60 then "\u001b[33m" else "" end)
    + . + reset;

  def tokens:
    (. / 1000 | round) as $k
    | if $k >= 1000 then "\($k / 100 | round / 10)M" else "\($k)k" end;

  # Gradient stops in OKLab, interpolating there keeps the gradient perceptually uniform
  def green: [0.7289, -0.0678, 0.0733]; # 8fb573
  def yellow: [0.7939, 0.0133, 0.0964]; # dbb671
  def red: [0.6444, 0.1538, 0.0491]; # de5d68

  def mix($from; $to; $t): [range(3) | $from[.] + ($to[.] - $from[.]) * $t];

  def oklab_to_srgb:
    . as [$L, $a, $b]
    | [
        $L + 0.3963377774 * $a + 0.2158037573 * $b,
        $L - 0.1055613458 * $a - 0.0638541728 * $b,
        $L - 0.0894841775 * $a - 1.2914855480 * $b
      ]
    | map(. * . * .) as [$l, $m, $s]
    | [
        4.0767416621 * $l - 3.3077115913 * $m + 0.2309699292 * $s,
        -1.2684380046 * $l + 2.6097574011 * $m - 0.3413193965 * $s,
        -0.0041960863 * $l - 0.7034186147 * $m + 1.7076147010 * $s
      ]
    | map(
        [., 0] | max
        | if . <= 0.0031308 then 12.92 * . else 1.055 * pow(.; 1 / 2.4) - 0.055 end
        | [. * 255 | round, 255] | min
      );

  # Green up to 200k tokens, yellow at 300k, red from 400k
  def context_color:
    if . <= 200000 then green
    elif . < 300000 then mix(green; yellow; (. - 200000) / 100000)
    elif . < 400000 then mix(yellow; red; (. - 300000) / 100000)
    else red end
    | oklab_to_srgb
    | "\u001b[38;2;\(.[0]);\(.[1]);\(.[2])m";

  def countdown:
    ([. - now, 0] | max | floor) as $s
    | ($s / 3600 | floor) as $h
    | ($s % 3600 / 60 | floor) as $m
    | if $h > 0 then "\($h)h\($m)m" else "\($m)m" end;

  def snowflake: "\udb81\udf17"; # nf-md-snowflake
  def blue: "\u001b[38;2;87;165;229m"; # 57a5e5

  # Seconds until the prompt cache goes cold, null when caching is not in use
  def cache_remaining:
    .prompt_cache // {} | if .caching_observed then (.expires_at // 0) - now else null end;

  # Hidden for the first 10 minutes after the cache was last touched, then counts down to expiry
  def cache_countdown($remaining):
    ({"5m": 300, "1h": 3600}[.ttl // ""] // 3600) as $ttl
    | select($remaining > 0 and $remaining <= $ttl - 600)
    | (if $remaining <= 300 then "\u001b[33m" else blue end)
      + snowflake + " in " + (.expires_at | countdown) + reset;

  def limit(resets_at_format):
    select(.used_percentage != null)
    | .used_percentage as $used_percentage
    | ("\($used_percentage | floor)%" | colored($used_percentage))
      + (if .resets_at then " ↻" + (.resets_at | resets_at_format) | dim else "" end);

  cache_remaining as $cache_remaining
  | [
    ([.model.display_name, .effort.level] | map(select(. != null)) | join(" ")),
    (.context_window
     | (.total_input_tokens // 0) as $used
     | "\($used | tokens)/\(.context_window_size // 0 | tokens)"
     # With a cold cache the next request re-caches the whole context
     | if $cache_remaining != null and $cache_remaining <= 0
       then "\u001b[1m" + blue + snowflake + " " + . else ($used | context_color) + . end
     | . + reset),
    (select($cache_remaining != null) | .prompt_cache | cache_countdown($cache_remaining)),
    (.rate_limits.five_hour // empty | limit(countdown)),
    (.rate_limits.seven_day // empty | limit(strflocaltime("%a %H:%M"))),
    (select($branch != "") | "\ue0a0 " + $branch),
    (.session_name // empty)
  ]
  | join(" · " | dim)
' <<<"$input"
