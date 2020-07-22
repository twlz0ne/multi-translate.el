# multi-translate.el

Translate word or region at point with multiple translation services.

Currently supports bing/google/youdao/sdcv.

![](images/multi-translate-word.png)

![](images/multi-translate-region.png)

## Installation

```elisp
(quelpa '(multi-translate
          :fetcher github
          :repo "twlz0ne/multi-translate.el"
          :files ("multi-translate.el")))
```

## Usage

- `multi-translate`

    Translate text from input.

- `multi-translate-at-point`

    Translate word or region.

- `multi-translate-amend-query`

    Amend current query and resubmit it.

## Use different backends translate words and sentences

Usally, a local dictionary is enough for words translating, only sentences need
to be send to remote. Following are the relevant custom variables:

- `multi-translate-word-backends` (default `'(sdcv)`)
- `multi-translate-sentence-backends` (default `'(bing google youdao)`)

## Async request

The async request is enabled by default (see `multi-translate-enable-async-request`). But not all backends support async:

| Backends                                                                        | Async              |
|:--------------------------------------------------------------------------------|:------------------:|
| bing([bing-dict](https://github.com/cute-jumper/bing-dict.el))                  | Yes                |
| google([google-translate](https://github.com/atykhonov/google-translate))       | No                 |
| youdao([youdao-dictionary](https://github.com/xuchunyang/youdao-dictionary.el)) | Yes                |
| sdcv([sdcv](http://www.emacswiki.org/emacs/download/sdcv.el))                   | No (not necessary) |

## Accumulate translate results

- `multi-translate-accumulate-results`

    Whether or not to accumulate translate results (default `nil`).

- `multi-translate-toggle-translation-section`

    Toggle current translation section hiding or showing.

- `multi-translate-{fold, open}-all-translation-section`

    Fold/open all translation sections.

- `multi-translate-{prev, next}-translation-section`

    Previous/next translation section

- `multi-translate-clean-buffer`

    Remove all translate results.
