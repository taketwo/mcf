" Vim syntax file
" Language:    JIRA wiki markup
" Reference:   https://jira.atlassian.com/secure/WikiRendererHelpAction.jspa?section=all

if exists("b:current_syntax")
  finish
endif

syn cluster jiraInline contains=jiraBold,jiraItalic,jiraInlineCode,jiraLink,jiraMention

" Emphasis must not start or end next to a word character or whitespace, so
" snake_case, list markers, and stray asterisks are left alone.
syn match jiraBold '\%(^\|\W\)\zs\*\S\%([^*]*\S\)\?\*\ze\%(\W\|$\)'
syn match jiraItalic '\%(^\|\W\)\zs_\S\%([^_]*\S\)\?_\ze\%(\W\|$\)'
syn match jiraInlineCode '{{.\{-}}}'
syn match jiraLink '\[[^]|~]*|[^]]\+\]'
syn match jiraLink '\[\a\+://[^]]\+\]'
syn match jiraMention '\[\~[^]]\+\]'

syn match jiraHeading '^\s*h[1-6]\.\s.*$'
syn match jiraListMarker '^\s*[*#-]\+\ze\s'
syn match jiraQuote '^\s*bq\.\s.*$' contains=@jiraInline
syn region jiraQuote start='{quote}' end='{quote}' contains=@jiraInline

" Delimiters are the pipes outside links, which the cluster consumes first.
syn match jiraTableRow '^\s*|.*$' contains=jiraTableDelim,@jiraInline
syn match jiraTableHeader '^\s*||.*$' contains=jiraTableDelim,@jiraInline
syn match jiraTableDelim '|\+' contained

syn region jiraCodeBlock start='{code\%(:[^}]*\)\?}' end='{code}' keepend
syn region jiraCodeBlock start='{noformat\%(:[^}]*\)\?}' end='{noformat}' keepend

syn sync fromstart

hi def link jiraBold          @markup.strong
hi def link jiraItalic        @markup.italic
hi def link jiraInlineCode    @markup.raw
hi def link jiraLink          @markup.link
hi def link jiraMention       Special
hi def link jiraHeading       @markup.heading
hi def link jiraListMarker    @markup.list
hi def link jiraQuote         @markup.quote
hi def link jiraTableHeader   @markup.strong
hi def link jiraTableDelim    Delimiter
hi def link jiraCodeBlock     @markup.raw.block

let b:current_syntax = "jira"
