
common_values = {
    'animation_direction': [
        'alternate', 'alternate-reverse', 'normal', 'reverse'
    ],
    'absolute_size': [
        'xx-small', 'x-small', 'small', 'medium', 'large', 'x-large', 'xx-large'
    ],
    'absolute_weight': [
        '100', '200', '300', '400', '500', '600', '700', '800', '900',
        'normal', 'bold'
    ],
    'basic_shape': [
        ['circle()', 'circle($1)'],
        ['ellipse()', 'ellipse($1)'],
        ['inset()', 'inset($1)'],
        ['polygon()', 'polygon($1)']
    ],
    'blend_mode': [
        'normal', 'multiply', 'screen', 'overlay', 'darken', 'lighten',
        'color-dodge', 'color-burn', 'hard-light', 'soft-light', 'difference',
        'exclusion', 'hue', 'saturation', 'color', 'luminosity'
    ],
    'border_style': [
        'none', 'hidden', 'dotted', 'dashed', 'solid', 'double',
        'groove', 'ridge', 'inset', 'outset'
    ],
    'border_width': ['thin', 'medium', 'thick'],
    'break_before_after': [
        'always', 'left', 'right', 'recto', 'verso', 'page', 'column', 'region'
    ],
    'break_inside': [
        'auto', 'avoid', 'avoid-page', 'avoid-column', 'avoid-region'
    ],
    'color': [
        'transparent',
        ['rgb()', 'rgb(${1:0}, ${2:0}, ${3:0})'],
        ['rgba()', 'rgba(${1:0}, ${2:0}, ${3:0}, ${4:1.0})'],
        ['hsl()', 'hsl(${1:0}, ${2:100%}, ${3:50%})'],
        ['hsla()', 'hsla(${1:0}, ${2:100%}, ${3:50%}, ${4:1.0})'],
        #named
        'AliceBlue',
        'AntiqueWhite',
        'Aqua',
        'Aquamarine',
        'Azure',
        'Beige',
        'Bisque',
        'Black',
        'BlanchedAlmond',
        'Blue',
        'BlueViolet',
        'Brown',
        'BurlyWood',
        'CadetBlue',
        'Chartreuse',
        'Chocolate',
        'Coral',
        'CornflowerBlue',
        'Cornsilk',
        'Crimson',
        'Cyan',
        'DarkBlue',
        'DarkCyan',
        'DarkGoldenRod',
        'DarkGray',
        'DarkGreen',
        'DarkGrey',
        'DarkKhaki',
        'DarkMagenta',
        'DarkOliveGreen',
        'DarkOrchid',
        'DarkRed',
        'DarkSalmon',
        'DarkSeaGreen',
        'DarkSlateBlue',
        'DarkSlateGray',
        'DarkSlateGrey',
        'DarkTurquoise',
        'DarkViolet',
        'Darkorange',
        'DeepPink',
        'DeepSkyBlue',
        'DimGray',
        'DimGrey',
        'DodgerBlue',
        'FireBrick',
        'FloralWhite',
        'ForestGreen',
        'Fuchsia',
        'Gainsboro',
        'GhostWhite',
        'Gold',
        'GoldenRod',
        'Gray',
        'Green',
        'GreenYellow',
        'Grey',
        'HoneyDew',
        'HotPink',
        'IndianRed',
        'Indigo',
        'Ivory',
        'Khaki',
        'Lavender',
        'LavenderBlush',
        'LawnGreen',
        'LemonChiffon',
        'LightBlue',
        'LightCoral',
        'LightCyan',
        'LightGoldenRodYellow',
        'LightGray',
        'LightGreen',
        'LightGrey',
        'LightPink',
        'LightSalmon',
        'LightSeaGreen',
        'LightSkyBlue',
        'LightSlateGray',
        'LightSlateGrey',
        'LightSteelBlue',
        'LightYellow',
        'Lime',
        'LimeGreen',
        'Linen',
        'Magenta',
        'Maroon',
        'MediumAquaMarine',
        'MediumBlue',
        'MediumOrchid',
        'MediumPurple',
        'MediumSeaGreen',
        'MediumSlateBlue',
        'MediumSpringGreen',
        'MediumTurquoise',
        'MediumVioletRed',
        'MidnightBlue',
        'MintCream',
        'MistyRose',
        'Moccasin',
        'NavajoWhite',
        'Navy',
        'OldLace',
        'Olive',
        'OliveDrab',
        'Orange',
        'OrangeRed',
        'Orchid',
        'PaleGoldenRod',
        'PaleGreen',
        'PaleTurquoise',
        'PaleVioletRed',
        'PapayaWhip',
        'PeachPuff',
        'Peru',
        'Pink',
        'Plum',
        'PowderBlue',
        'Purple',
        'RebeccaPurple',
        'Red',
        'RosyBrown',
        'RoyalBlue',
        'SaddleBrown',
        'Salmon',
        'SandyBrown',
        'SeaGreen',
        'SeaShell',
        'Sienna',
        'Silver',
        'SkyBlue',
        'SlateBlue',
        'SlateGray',
        'SlateGrey',
        'Snow',
        'SpringGreen',
        'SteelBlue',
        'Tan',
        'Teal',
        'Thistle',
        'Tomato',
        'Transparent',
        'Turquoise',
        'Violet',
        'Wheat',
        'White',
        'WhiteSmoke',
        'Yellow',
        'YellowGreen',
        #special
        'ActiveText',
        'ButtonBorder',
        'ButtonFace',
        'ButtonText',
        'Canvas',
        'CanvasText',
        'CurrentColor',
        'Field',
        'FieldText',
        'GrayText',
        'Highlight',
        'HighlightText',
        'LinkText',
        'Mark',
        'MarkText',
        'VisitedText',
    ],
    'counter_symbols': [
        'cyclic', 'numeric', 'alphabetic', 'symbolic', 'additive', 'fixed'
    ],
    'font_variant_alternates': [
        'normal', 'historical-forms',
        ['annotation()', 'annotation($1)'],
        ['character-variant()', 'character-variant($1)'],
        ['ornaments()', 'ornaments($1)'],
        ['styleset()', 'styleset($1)'],
        ['stylistic()', 'stylistic($1)'],
        ['swash()', 'swash($1)'],
    ],
    'generic_name': [
        'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
    ],
    'gradient': [
        ['conic-gradient()', 'conic-gradient($1)'],
        ['linear-gradient()', 'linear-gradient($1)'],
        ['radial-gradient()', 'radial-gradient($1)'],
        ['repeating-conic-gradient()', 'repeating-conic-gradient($1)'],
        ['repeating-linear-gradient()', 'repeating-linear-gradient($1)'],
        ['repeating-radial-gradient()', 'repeating-radial-gradient($1)']
    ],
    'grid': [
        ['minmax()', 'minmax(${1:100px}, ${2:1fr})'],
        ['repeat()', 'repeat(${1:2}, ${2:1fr})'],
    ],
    'justify': [
        "start", "end", "flex-start", "flex-end", "center", "left", "right",
        "safe start", "safe end", "safe flex-start", "safe flex-end",
        "safe center", "safe left", "safe right", "unsafe start",
        "unsafe end", "unsafe flex-start", "unsafe flex-end",
        "unsafe center", "unsafe left", "unsafe right", "normal",
        "baseline", "first baseline", "last baseline", "space-between",
        "space-around", "space-evenly", "stretch", "legacy",
        "lecacy center", "legacy left", "legacy right"
    ],
    'list_style_type': [
        'none', 'inline', 'disc', 'circle', 'square', 'decimal',
        'decimal-leading-zero', 'arabic-indic', 'binary', 'bengali',
        'cambodian', 'khmer', 'devanagari', 'gujarati', 'gurmukhi',
        'kannada', 'lower-hexadecimal', 'lao', 'malayalam', 'mongolian',
        'myanmar', 'octal', 'oriya', 'persian', 'urdu', 'telugu',
        'tibetan', 'thai', 'upper-hexadecimal', 'lower-roman',
        'upper-roman', 'lower-greek', 'lower-alpha', 'lower-latin',
        'upper-alpha', 'upper-latin', 'afar', 'ethiopic-halehame-aa-et',
        'ethiopic-halehame-aa-er', 'amharic', 'ethiopic-halehame-am-et',
        'amharic-abegede', 'ethiopic-abegede-am-et', 'cjk-earthly-branch',
        'cjk-heavenly-stem', 'ethiopic', 'ethiopic-halehame-gez',
        'ethiopic-abegede', 'ethiopic-abegede-gez', 'hangul-consonant',
        'hangul', 'lower-norwegian', 'oromo', 'ethiopic-halehame-om-et',
        'sidama', 'ethiopic-halehame-sid-et', 'somali',
        'ethiopic-halehame-so-et', 'tigre', 'ethiopic-halehame-tig',
        'tigrinya-er', 'ethiopic-halehame-ti-er', 'tigrinya-er-abegede',
        'ethiopic-abegede-ti-er', 'tigrinya-et', 'ethiopic-halehame-ti-et',
        'tigrinya-et-abegede', 'ethiopic-abegede-ti-et', 'upper-greek',
        'upper-norwegian', 'asterisks', 'footnotes', 'hebrew', 'armenian',
        'lower-armenian', 'upper-armenian', 'georgian', 'cjk-ideographic',
        'hiragana', 'katakana', 'hiragana-iroha', 'katakana-iroha'
    ],
    'position': ['top', 'right', 'bottom', 'left', 'center'],
    'relative_size': ['larger', 'smaller'],
    'relative_weight': ['bolder', 'lighter'],
    'repeat_style': ['no-repeat', 'repeat', 'repeat-x', 'repeat-y'],
    'snap': [
        ["snapList()", "snapList($1)"],
        ["snapInterval()", "snapInterval(${1:100%}, ${1:100%})"],
    ],
    'string': ['\"$1\"'],
    'timing': [
        'linear',
        'ease', 'ease-in', 'ease-out', 'ease-in-out', 'step-start', 'step-end',
        ['cubic-bezier()', 'cubic-bezier(${1:0.0}, ${2:0.0}, ${3:1.0}, ${4:1.0})'],
        ['steps()', 'steps(${1:2}, ${2:start})'],
    ],
    'uri': [['url()', 'url($1)']],
}

properties_dict = {
    '-moz-background-inline-policy': [
        'bounding-box',
        'continuous',
        'each-box'
    ],
    '-moz-binding': [
        'none'
    ],
    '-moz-border-bottom-colors': [
        'none'
    ],
    '-moz-border-left-colors': [
        'none'
    ],
    '-moz-border-right-colors': [
        'none'
    ],
    '-moz-border-top-colors': [
        'none'
    ],
    '-moz-box-flexgroup': [],
    '-moz-context-properties': [
        'fill',
        'fill-opacity',
        'none',
        'stroke',
        'stroke-opacity'
    ],
    '-moz-float-edge': [
        'border-box',
        'content-box',
        'margin-box',
        'padding-box'
    ],
    '-moz-force-broken-image-icon': [],
    '-moz-image-region': [
        'auto'
    ],
    '-moz-orient': [
        'block',
        'horizontal',
        'inline',
        'vertical'
    ],
    '-moz-outline-radius': [],
    '-moz-outline-radius-bottomleft': [],
    '-moz-outline-radius-bottomright': [],
    '-moz-outline-radius-topleft': [],
    '-moz-outline-radius-topright': [],
    '-moz-stack-sizing': [
        'ignore',
        'stretch-to-fit'
    ],
    '-moz-text-blink': [
        'blink',
        'none'
    ],
    '-moz-user-focus': [
        'ignore',
        'none',
        'normal',
        'select-after',
        'select-all',
        'select-before',
        'select-menu',
        'select-same'
    ],
    '-moz-user-input': [
        'auto',
        'disabled',
        'enabled',
        'none'
    ],
    '-moz-user-modify': [
        'read-only',
        'read-write',
        'write-only'
    ],
    '-moz-window-dragging': [
        'drag',
        'no-drag'
    ],
    '-moz-window-shadow': [
        'default',
        'menu',
        'none',
        'sheet',
        'tooltip'
    ],
    '-ms-accelerator': [
        'false',
        'true'
    ],
    '-ms-block-progression': [
        'bt',
        'lr',
        'rl',
        'tb'
    ],
    '-ms-content-zoom-chaining': [
        'chained',
        'none'
    ],
    '-ms-content-zoom-limit': [],
    '-ms-content-zoom-limit-max': [],
    '-ms-content-zoom-limit-min': [],
    '-ms-content-zoom-snap': [
        '<snap>',
        'mandatory',
        'none',
        'proximity'
    ],
    '-ms-content-zoom-snap-points': [
        '<snap>'
    ],
    '-ms-content-zoom-snap-type': [
        'mandatory',
        'none',
        'proximity'
    ],
    '-ms-content-zooming': [
        'none',
        'zoom'
    ],
    '-ms-flex-align': [
        'baseline',
        'center',
        'end',
        'start',
        'stretch'
    ],
    '-ms-flex-item-align': [
        'auto',
        'baseline',
        'center',
        'end',
        'start',
        'stretch'
    ],
    '-ms-flex-line-pack': [
        'center',
        'distribute',
        'end',
        'justify',
        'start',
        'stretch'
    ],
    '-ms-flex-order': [],
    '-ms-flex-pack': [
        'center',
        'distribute',
        'end',
        'justify',
        'start'
    ],
    '-ms-flow-from': [
        'none'
    ],
    '-ms-flow-into': [
        'none'
    ],
    '-ms-grid-column-align': [
        'center',
        'end',
        'start',
        'stretch'
    ],
    '-ms-grid-column-span': [],
    '-ms-grid-columns': [
        'none'
    ],
    '-ms-grid-layer': [],
    '-ms-grid-row-align': [
        'center',
        'end',
        'start',
        'stretch'
    ],
    '-ms-grid-row-span': [],
    '-ms-grid-rows': [
        'none'
    ],
    '-ms-high-contrast-adjust': [
        'auto',
        'none'
    ],
    '-ms-hyphenate-limit-chars': [
        'auto'
    ],
    '-ms-hyphenate-limit-lines': [
        'no-limit'
    ],
    '-ms-hyphenate-limit-zone': [],
    '-ms-ime-align': [
        'after',
        'auto'
    ],
    '-ms-interpolation-mode': [
        'bicubic',
        'nearest-neighbor'
    ],
    '-ms-layout-grid': [
        'char',
        'line',
        'mode',
        'type'
    ],
    '-ms-layout-grid-char': [
        'auto',
        'none'
    ],
    '-ms-layout-grid-line': [
        'auto',
        'none'
    ],
    '-ms-layout-grid-mode': [
        'both',
        'char',
        'line',
        'none'
    ],
    '-ms-layout-grid-type': [
        'fixed',
        'loose',
        'strict'
    ],
    '-ms-overflow-style': [
        '-ms-autohiding-scrollbar',
        'auto',
        'none',
        'scrollbar'
    ],
    '-ms-perspective-origin-x': [],
    '-ms-perspective-origin-y': [],
    '-ms-progress-appearance': [
        'bar',
        'ring'
    ],
    '-ms-scroll-chaining': [
        'chained',
        'none'
    ],
    '-ms-scroll-limit': [
        'auto'
    ],
    '-ms-scroll-limit-x-max': [
        'auto'
    ],
    '-ms-scroll-limit-x-min': [],
    '-ms-scroll-limit-y-max': [
        'auto'
    ],
    '-ms-scroll-limit-y-min': [],
    '-ms-scroll-rails': [
        'none',
        'railed'
    ],
    '-ms-scroll-snap-x': [
        '<snap>',
        'mandatory',
        'none',
        'proximity'
    ],
    '-ms-scroll-snap-y': [
        '<snap>',
        'mandatory',
        'none',
        'proximity'
    ],
    '-ms-scroll-translation': [
        'none',
        'vertical-to-horizontal'
    ],
    '-ms-text-autospace': [
        'ideograph-alpha',
        'ideograph-numeric',
        'ideograph-parenthesis',
        'ideograph-space',
        'none',
        'punctuation'
    ],
    '-ms-text-combine-horizontal': [
        'all',
        'digits',
        'none'
    ],
    '-ms-text-kashida-space': [],
    '-ms-touch-select': [
        'grippers',
        'none'
    ],
    '-ms-transform-origin-x': [],
    '-ms-transform-origin-y': [],
    '-ms-transform-origin-z': [],
    '-ms-wrap-flow': [
        'auto',
        'both',
        'clear',
        'end',
        'maximum',
        'minimum',
        'start'
    ],
    '-ms-wrap-margin': [],
    '-ms-wrap-through': [
        'none',
        'wrap'
    ],
    '-ms-zoom-animation': [
        'default',
        'none'
    ],
    '-o-table-baseline': [],
    '-webkit-background-composite': [
        'border',
        'padding'
    ],
    '-webkit-border-before': [],
    '-webkit-border-before-color': [],
    '-webkit-border-before-style': [],
    '-webkit-border-before-width': [],
    '-webkit-box-reflect': [
        'above',
        'below',
        'left',
        'right'
    ],
    '-webkit-column-break-after': [
        'always',
        'auto',
        'avoid',
        'avoid-column',
        'avoid-page',
        'avoid-region',
        'column',
        'left',
        'page',
        'region',
        'right'
    ],
    '-webkit-column-break-before': [
        'always',
        'auto',
        'avoid',
        'avoid-column',
        'avoid-page',
        'avoid-region',
        'column',
        'left',
        'page',
        'region',
        'right'
    ],
    '-webkit-column-break-inside': [
        'auto',
        'avoid',
        'avoid-column',
        'avoid-page',
        'avoid-region'
    ],
    '-webkit-flow-from': [
        'none'
    ],
    '-webkit-flow-into': [
        'none'
    ],
    '-webkit-margin-bottom-collapse': [
        'collapse',
        'discard',
        'separate'
    ],
    '-webkit-margin-collapse': [
        'collapse',
        'discard',
        'separate'
    ],
    '-webkit-margin-start': [
        'auto'
    ],
    '-webkit-margin-top-collapse': [
        'collapse',
        'discard',
        'separate'
    ],
    '-webkit-mask-attachment': [],
    '-webkit-mask-position-x': [
        'center',
        'left',
        'right'
    ],
    '-webkit-mask-position-y': [
        'bottom',
        'center',
        'top'
    ],
    '-webkit-mask-repeat-x': [
        'no-repeat',
        'repeat',
        'round',
        'space'
    ],
    '-webkit-mask-repeat-y': [
        'no-repeat',
        'repeat',
        'round',
        'space'
    ],
    '-webkit-nbsp-mode': [
        'normal',
        'space'
    ],
    '-webkit-overflow-scrolling': [
        'auto',
        'touch'
    ],
    '-webkit-padding-start': [],
    '-webkit-region-fragment': [
        'auto',
        'break'
    ],
    '-webkit-tap-highlight-color': [],
    '-webkit-text-fill-color': [],
    '-webkit-text-stroke': [],
    '-webkit-text-stroke-color': [],
    '-webkit-text-stroke-width': [],
    '-webkit-touch-callout': [
        'default',
        'none'
    ],
    '-webkit-transform-origin-x': [],
    '-webkit-transform-origin-y': [],
    '-webkit-transform-origin-z': [],
    '-webkit-user-drag': [
        'auto',
        'element',
        'none'
    ],
    '-webkit-user-modify': [
        'read-only',
        'read-write',
        'read-write-plaintext-only'
    ],
    'additive-symbols': [],
    'align-content': [
        'center',
        'flex-end',
        'flex-start',
        'space-around',
        'space-between',
        'stretch'
    ],
    'align-items': [
        'baseline',
        'center',
        'flex-end',
        'flex-start',
        'stretch'
    ],
    'align-self': [
        'auto',
        'baseline',
        'center',
        'flex-end',
        'flex-start',
        'stretch'
    ],
    'align-tracks': [
        'normal'
    ],
    'all': [
        'revert'
    ],
    'alt': [],
    'animation': [
        '<animation_direction>',
        '<timing>',
        'none',
        'infinite',
        'forwards',
        'backwards',
        'both',
        'running',
        'paused'
    ],
    'animation-delay': [],
    'animation-direction': [
        '<animation_direction>'
    ],
    'animation-duration': [],
    'animation-fill-mode': [
        'none',
        'forwards',
        'backwards',
        'both'
    ],
    'animation-iteration-count': [
        'infinite',
    ],
    'animation-name': [
        'none',
    ],
    'animation-play-state': [
        'running',
        'paused'
    ],
    'animation-timing-function': [
        '<timing>'
    ],
    'appearance': [
        'auto',
        'menulist-button',
        'none',
        'textfield'
    ],
    'aspect-ratio': [
        'auto'
    ],
    'azimuth': [
        'behind',
        'center',
        'center-left',
        'center-right',
        'far-left',
        'far-right',
        'left',
        'left-side',
        'leftwards',
        'right',
        'right-side',
        'rightwards'
    ],
    'backdrop-filter': [
        'none'
    ],
    'backface-visibility': [
        'visible',
        'hidden'
    ],
    'background': [
        '<color>',
        '<gradient>',
        '<position>',
        '<repeat_style>'
        '<uri>',
        'fixed',
        'scroll',
    ],
    'background-attachment': [
        'fixed',
        'local',
        'scroll'
    ],
    'background-blend-mode': [
        '<blend_mode>'
    ],
    'background-clip': [
        'border-box',
        'content-box',
        'padding-box',
    ],
    'background-color': [
        '<color>'
    ],
    'background-image': [
        '<uri>',
        'none'
    ],
    'background-origin': [
        'border-box',
        'content-box',
        'padding-box',
    ],
    'background-position': [
        '<position>'
    ],
    'background-position-x': [
        'center',
        'left',
        'right',
        'x-end',
        'x-start'
    ],
    'background-position-y': [
        'bottom',
        'center',
        'top',
        'y-end',
        'y-start'
    ],
    'background-repeat': [
        '<repeat_style>',
        'space',
        'round',
    ],
    'background-size': [
        'auto',
        'cover',
        'contain',
    ],
    'behavior': [],
    'bleed': [
        'auto'
    ],
    'block-size': [
        'auto'
    ],
    'border': [
        '<border_width>',
        '<border_style>',
        '<color>'
    ],
    'border-block': [],
    'border-block-color': [],
    'border-block-end': [],
    'border-block-end-color': [],
    'border-block-end-style': [],
    'border-block-end-width': [],
    'border-block-start': [],
    'border-block-start-color': [],
    'border-block-start-style': [],
    'border-block-start-width': [],
    'border-block-style': [],
    'border-block-width': [],
    'border-bottom': [
        '<border_width>',
        '<border_style>',
        '<color>'
    ],
    'border-bottom-color': [
        '<color>'
    ],
    'border-bottom-left-radius': [],
    'border-bottom-right-radius': [],
    'border-bottom-style': [
        '<border_style>'
    ],
    'border-bottom-width': [
        '<border_width>'
    ],
    'border-collapse': [
        'collapse',
        'separate'
    ],
    'border-color': [
        '<color>'
    ],
    'border-end-end-radius': [],
    'border-end-start-radius': [],
    'border-image': [
        '<border-image-source>',
        '<border-image-slice>',
        '<border-image-width>',
        '<border-image-outset>',
        '<border-image-repeat>'
    ],
    'border-image-outset': [],
    'border-image-repeat': [
        'stretch',
        'repeat',
        'round',
        'space'
    ],
    'border-image-slice': [
        'fill',
    ],
    'border-image-source': [
        'none',
        '<image>'
    ],
    'border-image-width': [
        'auto'
    ],
    'border-inline': [],
    'border-inline-color': [],
    'border-inline-end': [],
    'border-inline-end-color': [],
    'border-inline-end-style': [],
    'border-inline-end-width': [],
    'border-inline-start': [],
    'border-inline-start-color': [],
    'border-inline-start-style': [],
    'border-inline-start-width': [],
    'border-inline-style': [],
    'border-inline-width': [],
    'border-left': [
        '<border_width>',
        '<border_style>',
        '<color>'
    ],
    'border-left-color': [
        '<color>'
    ],
    'border-left-style': [
        '<border_style>'
    ],
    'border-left-width': [
        '<border_width>'
    ],
    'border-radius': [],
    'border-right': [
        '<border_width>',
        '<border_style>',
        '<color>'
    ],
    'border-right-color': [
        '<color>'
    ],
    'border-right-style': [
        '<border_style>'
    ],
    'border-right-width': [
        '<border_width>'
    ],
    'border-spacing': [],
    'border-start-end-radius': [],
    'border-start-start-radius': [],
    'border-style': [
        '<border_style>'
    ],
    'border-top': [
        '<border_width>',
        '<border_style>',
        '<color>'
    ],
    'border-top-color': [
        '<color>'
    ],
    'border-top-left-radius': [],
    'border-top-right-radius': [],
    'border-top-style': [
        '<border_style>'
    ],
    'border-top-width': [
        '<border_width>'
    ],
    'border-width': [
        '<border_width>'
    ],
    'bottom': [
        'auto'
    ],
    'box-align': [
        'baseline',
        'center',
        'end',
        'start',
        'stretch'
    ],
    'box-decoration-break': [
        'slice',
        'clone'
    ],
    'box-direction': [
        'normal',
        'reverse'
    ],
    'box-flex': [],
    'box-flex-group': [],
    'box-lines': [
        'multiple',
        'single'
    ],
    'box-ordinal-group': [],
    'box-orient': [
        'block-axis',
        'horizontal',
        'inline-axis',
        'vertical'
    ],
    'box-pack': [
        'center',
        'end',
        'justify',
        'start'
    ],
    'box-shadow': [
        'none',
        'inset',
        '<color>'
    ],
    'box-sizing': [
        'content-box',
        'border-box'
    ],
    'break-after': [
        '<break_before_after>',
        '<break_inside>'
    ],
    'break-before': [
        '<break_before_after>',
        '<break_inside>'
    ],
    'break-inside': [
        '<break_inside>'
    ],
    'caption-side': [
        'top',
        'bottom'
    ],
    'caret-color': [
        'auto'
    ],
    'clear': [
        'none',
        'left',
        'right',
        'both'
    ],
    'clip': [
        ['rect()', 'rect(${1:0}, ${2:0}, ${3:0}, ${4:0})'],
        'auto'
    ],
    'clip-path': [
        'none',
        '<uri>',
        '<basic_shape>'
    ],
    'clip-rule': [
        'nonzero',
        'evenodd'
    ],
    'color': [
        '<color>'
    ],
    'color-adjust': [
        'economy',
        'exact'
    ],
    'color-interpolation-filters': [
        'auto',
        'sRGB',
        'linearRGB'
    ],
    'column-count': [
        'auto',
    ],
    'column-fill': [
        'auto',
        'balance'
    ],
    'column-gap': [
        'normal',
    ],
    'column-rule': [
        '<border_width>',
        '<border_style>',
        '<color>'
    ],
    'column-rule-color': [
        '<color>'
    ],
    'column-rule-style': [
        '<border_style>'
    ],
    'column-rule-width': [
        '<border_width>'
    ],
    'column-span': [
        'none'
    ],
    'column-width': [
        'auto',
    ],
    'columns': [
        'auto'
    ],
    'contain': [
        'content',
        'layout',
        'none',
        'paint',
        'size',
        'strict',
        'style'
    ],
    'content': [
        'none',
        'normal',
        '<string>',
        '<uri>',
        'open-quote',
        'close-quote',
        'no-open-quote',
        'no-close-quote',
        ['attr()', 'attr($1)'],
        ['counter()', 'counter($1)']
    ],
    'content-visibility': [
        'auto',
        'hidden',
        'visible'
    ],
    'counter-increment': [
        'none',
    ],
    'counter-reset': [
        'none',
    ],
    'counter-set': [
        'none'
    ],
    'cursor': [
        '<uri>',
        'auto',
        'default',
        'none',
        'context-menu',
        'help',
        'pointer',
        'progress',
        'wait',
        'cell',
        'crosshair',
        'text',
        'vertical-text',
        'alias',
        'copy',
        'move',
        'no-drop',
        'not-allowed',
        'e-resize',
        'n-resize',
        'ne-resize',
        'nw-resize',
        's-resize',
        'se-resize',
        'sw-resize',
        'w-resize',
        'ew-resize',
        'ns-resize',
        'nesw-resize',
        'nwse-resize',
        'col-resize',
        'row-resize',
        'all-scroll',
        'zoom-in',
        'zoom-out'
    ],
    'direction': [
        'ltr',
        'rtl'
    ],
    'display': [
        'none',
        'inline',
        'block',
        'contents',
        'list-item',
        'inline-block',
        'inline-table',
        'table',
        'table-cell',
        'table-column',
        'table-column-group',
        'table-footer-group',
        'table-header-group',
        'table-row',
        'table-row-group',
        'table-caption',
        'flex',
        'inline-flex',
        'grid',
        'inline-grid',
        'ruby',
        'ruby-base',
        'ruby-text',
        'ruby-base-container',
        'ruby-text-container',
        'run-in'
    ],
    'empty-cells': [
        'show',
        'hide'
    ],
    'enable-background': [
        'accumulate',
        'new'
    ],
    'fallback': [],
    'fill': [
        '<color>'
    ],
    'fill-opacity': [],
    'fill-rule': [
        'nonzero',
        'evenodd'
    ],
    'filter': [
        '<uri>',
        ['blur()', 'blur(${1:5px})'],
        ['brightness()', 'brightness(${1:1.0})'],
        ['contrast()', 'contrast(${1:100%})'],
        ['drop-shadow()', 'drop-shadow(${1:1px} ${2:1px})'],
        ['grayscale()', 'grayscale(${1:50%})'],
        ['hue-rotate()', 'hue-rotate(${1:90deg})'],
        ['invert()', 'invert(${1:50%})'],
        ['opacity()', 'opacity(${1:100%})'],
        ['saturate()', 'saturate(${1:50%})'],
        ['sepia()', 'sepia(${1:50%})']
    ],
    'flex': [
        'none'
    ],
    'flex-basis': [
        'auto',
    ],
    'flex-direction': [
        'row',
        'row-reverse',
        'column',
        'column-reverse'
    ],
    'flex-flow': [
        'row',
        'row-reverse',
        'column',
        'column-reverse',
        'nowrap',
        'wrap',
        'wrap-reverse'
    ],
    'flex-grow': [],
    'flex-shrink': [],
    'flex-wrap': [
        'nowrap',
        'wrap',
        'wrap-reverse'
    ],
    'float': [
        'left',
        'right',
        'none'
    ],
    'flood-color': [
        '<color>'
    ],
    'flood-opacity': [],
    'font': [
        '<absolute_weight>',
        '<generic_name>',
        '<relative_weight>',
        'caption',
        'icon',
        'italic',
        'menu',
        'message-box',
        'oblique',
        'small-caps',
        'small-caption',
        'status-bar'
    ],
    'font-display': [
        'auto',
        'block',
        'fallback',
        'optional',
        'swap'
    ],
    'font-family': [
        '<generic_name>'
    ],
    'font-feature-settings': [
        'normal',
        '<string>'
    ],
    'font-kerning': [
        'auto',
        'normal',
        'none'
    ],
    'font-language-override': [
        'normal',
        '<string>'
    ],
    'font-optical-sizing': [
        'auto',
        'none'
    ],
    'font-size': [
        '<absolute_size>',
        '<relative_size>',
    ],
    'font-size-adjust': [
        'none',
    ],
    'font-smooth': [
        'always',
        'auto',
        'never'
    ],
    'font-stretch': [
        'normal',
        'semi-condensed',
        'condensed',
        'extra-condensed',
        'ultra-condensed',
        'semi-expanded',
        'expanded',
        'extra-expanded',
        'ultra-expanded'
    ],
    'font-style': [
        'normal',
        'italic',
        'oblique'
    ],
    'font-synthesis': [
        'none',
        'weight',
        'style'
    ],
    'font-variant': [
        'normal',
        'small-caps'
    ],
    'font-variant-alternates': [
        '<font_variant_alternates>'
    ],
    'font-variant-caps': [
        'normal',
        'small-caps',
        'all-small-caps',
        'petite-caps',
        'all-petite-caps',
        'unicase',
        'titling-case'
    ],
    'font-variant-east-asian': [
        'normal',
        'ruby',
        'jis78',
        'jis83',
        'jis90',
        'jis04',
        'simplified',
        'traditional'
    ],
    'font-variant-ligatures': [
        'normal',
        'none',
        'common-ligatures',
        'no-common-ligatures',
        'discretionary-ligatures',
        'no-discretionary-ligatures',
        'historical-ligatures',
        'no-historical-ligatures',
        'contextual',
        'no-contextual'
    ],
    'font-variant-numeric': [
        'normal',
        'ordinal',
        'slashed-zero',
        'lining-nums',
        'oldstyle-nums',
        'proportional-nums',
        'tabular-nums',
        'diagonal-fractions',
        'stacked-fractions'
    ],
    'font-variant-position': [
        'normal',
        'sub',
        'super'
    ],
    'font-variation-settings': [
        'normal'
    ],
    'font-weight': [
        '<absolute_weight>',
        '<relative_weight>'
    ],
    'forced-color-adjust': [
        'auto',
        'none'
    ],
    'gap': [],
    'glyph-orientation-horizontal': [],
    'glyph-orientation-vertical': [
        'auto'
    ],
    'grid': [],
    'grid-area': [],
    'grid-auto-columns': [
        'auto',
    ],
    'grid-auto-flow': [
        'row',
        'column',
        'dense'
    ],
    'grid-auto-rows': [
        'auto',
    ],
    'grid-column': [],
    'grid-column-end': [],
    'grid-column-gap': [],
    'grid-column-start': [],
    'grid-gap': [],
    'grid-row': [],
    'grid-row-end': [],
    'grid-row-gap': [],
    'grid-row-start': [],
    'grid-template': [
        '<grid>',
        'auto',
        'max-content',
        'min-content',
        'none',
        'subgrid'
    ],
    'grid-template-areas': [],
    'grid-template-columns': [
        'auto',
        '<grid>',
    ],
    'grid-template-rows': [
        'auto',
        '<grid>',
    ],
    'hanging-punctuation': [
        'allow-end',
        'first',
        'force-end',
        'last',
        'none'
    ],
    'height': [
        'auto',
        'fit-content'
    ],
    'hyphens': [
        'none',
        'manual',
        'auto'
    ],
    'image-orientation': [
        'flip',
        'from-image'
    ],
    'image-rendering': [
        'auto',
        'optimizeSpeed',
        'optimizeQuality',
        'pixelated'
    ],
    'image-resolution': [
        'from-image'
    ],
    'ime-mode': [
        'auto',
        'normal',
        'active',
        'inactive',
        'disabled'
    ],
    'inherits': [
        'false',
        'true'
    ],
    'initial-letter': [
        'normal'
    ],
    'initial-letter-align': [
        'alphabetic',
        'auto',
        'hanging',
        'ideographic'
    ],
    'initial-value': [],
    'inline-size': [
        'auto'
    ],
    'inset': [],
    'inset-block': [],
    'inset-block-end': [],
    'inset-block-start': [],
    'inset-inline': [],
    'inset-inline-end': [],
    'inset-inline-start': [],
    'isolation': [
        'auto',
        'isolation'
    ],
    'justify-content': [
        '<justify>'
    ],
    'justify-items': [
        '<justify>'
    ],
    'justify-self': [],
    'justify-tracks': [
        'left',
        'normal',
        'right'
    ],
    'kerning': [
        'auto'
    ],
    'left': [
        'auto'
    ],
    'letter-spacing': [
        'normal',
    ],
    'lighting-color': [
        '<color>'
    ],
    'line-break': [
        'anywhere',
        'auto',
        'loose',
        'normal',
        'strict'
    ],
    'line-clamp': [
        'none'
    ],
    'line-height': [
        'normal',
    ],
    'line-height-step': [],
    'list-style': [
        '<list_style_type>',
        'inside',
        'outside',
        '<uri>'
    ],
    'list-style-image': [
        '<uri>',
        'none'
    ],
    'list-style-position': [
        'inside',
        'outside'
    ],
    'list-style-type': [
        '<list_style_type>'
    ],
    'margin': [
        'auto',
        '<margin-width>'
    ],
    'margin-block': [],
    'margin-block-end': [
        'auto'
    ],
    'margin-block-start': [
        'auto'
    ],
    'margin-bottom': [
        'auto',
        '<margin-width>'
    ],
    'margin-inline': [],
    'margin-inline-end': [
        'auto'
    ],
    'margin-inline-start': [
        'auto'
    ],
    'margin-left': [
        'auto',
        '<margin-width>'
    ],
    'margin-right': [
        'auto',
        '<margin-width>'
    ],
    'margin-top': [
        'auto',
        '<margin-width>'
    ],
    'margin-trim': [
        'all',
        'in-flow',
        'none'
    ],
    'marker': [
        '<uri>',
        'none'
    ],
    'marker-end': [
        '<uri>',
        'none'
    ],
    'marker-mid': [
        '<uri>',
        'none'
    ],
    'marker-start': [
        '<uri>',
        'none'
    ],
    'marks': [
        'crop',
        'cross',
        'none'
    ],
    'mask': [
        '<uri>',
        'none'
    ],
    'mask-border': [],
    'mask-border-mode': [
        'alpha',
        'luminance'
    ],
    'mask-border-outset': [],
    'mask-border-repeat': [
        'repeat',
        'round',
        'space',
        'stretch'
    ],
    'mask-border-slice': [],
    'mask-border-source': [
        'none'
    ],
    'mask-border-width': [
        'auto'
    ],
    'mask-clip': [
        'no-clip'
    ],
    'mask-composite': [],
    'mask-image': [
        '<uri>',
        'none'
    ],
    'mask-mode': [
        'alpha',
        'auto',
        'luminance'
    ],
    'mask-origin': [],
    'mask-position': [],
    'mask-repeat': [],
    'mask-size': [
        'auto',
        'contain',
        'cover'
    ],
    'mask-type': [
        'luminance',
        'alpha'
    ],
    'masonry-auto-flow': [
        'definite-first',
        'next',
        'ordered',
        'pack'
    ],
    'math-style': [
        'compact',
        'normal'
    ],
    'max-block-size': [
        'none'
    ],
    'max-height': [
        'fit-content',
        'none'
    ],
    'max-inline-size': [
        'none'
    ],
    'max-lines': [
        'none'
    ],
    'max-width': [
        'fit-content',
        'none'
    ],
    'max-zoom': [
        'auto'
    ],
    'min-block-size': [],
    'min-height': [
        'fit-content'
    ],
    'min-inline-size': [],
    'min-width': [
        'fit-content'
    ],
    'min-zoom': [
        'auto'
    ],
    'mix-blend-mode': [
        '<blend_mode>'
    ],
    'motion': [
        ['path()', 'path($1)'],
        'auto',
        'none',
        'reverse'
    ],
    'motion-offset': [],
    'motion-path': [
        ['path()', 'path($1)'],
        'none'
    ],
    'motion-rotation': [
        'auto',
        'reverse'
    ],
    'nav-down': [
        'auto',
        'current',
        'root'
    ],
    'nav-index': [
        'auto'
    ],
    'nav-left': [
        'auto',
        'current',
        'root'
    ],
    'nav-right': [
        'auto',
        'current',
        'root'
    ],
    'nav-up': [
        'auto',
        'current',
        'root'
    ],
    'negative': [],
    'object-fit': [
        'fill',
        'contain',
        'cover',
        'none',
        'scale-down'
    ],
    'object-position': [
        '<position>'
    ],
    'offset': [],
    'offset-anchor': [
        'auto'
    ],
    'offset-block-end': [
        'auto'
    ],
    'offset-block-start': [
        'auto'
    ],
    'offset-distance': [],
    'offset-inline-end': [
        'auto'
    ],
    'offset-inline-start': [
        'auto'
    ],
    'offset-path': [
        'none'
    ],
    'offset-position': [
        'auto'
    ],
    'offset-rotate': [
        'auto',
        'reverse'
    ],
    'opacity': [],
    'order': [],
    'orientation': [
        'auto',
        'landscape',
        'portrait'
    ],
    'orphans': [],
    'outline': [
        '<color>',
        '<border_style>',
        '<border_width>',
    ],
    'outline-color': [
        '<color>',
        'invert'
    ],
    'outline-offset': [],
    'outline-style': [
        '<border_style>'
    ],
    'outline-width': [
        '<border_width>',
    ],
    'overflow': [
        'visible',
        'hidden',
        'scroll',
        'auto'
    ],
    'overflow-anchor': [
        'auto',
        'none'
    ],
    'overflow-block': [
        'auto',
        'clip',
        'hidden',
        'scroll',
        'visible'
    ],
    'overflow-clip-box': [
        'content-box',
        'padding-box'
    ],
    'overflow-inline': [
        'auto',
        'clip',
        'hidden',
        'scroll',
        'visible'
    ],
    'overflow-wrap': [
        'normal',
        'break-word'
    ],
    'overflow-x': [
        'visible',
        'hidden',
        'scroll',
        'auto'
    ],
    'overflow-y': [
        'visible',
        'hidden',
        'scroll',
        'auto'
    ],
    'overscroll-behavior': [
        'auto',
        'contain',
        'none'
    ],
    'overscroll-behavior-block': [
        'auto',
        'contain',
        'none'
    ],
    'overscroll-behavior-inline': [
        'auto',
        'contain',
        'none'
    ],
    'overscroll-behavior-x': [
        'auto',
        'contain',
        'none'
    ],
    'overscroll-behavior-y': [
        'auto',
        'contain',
        'none'
    ],
    'pad': [],
    'padding': [
        'auto'
    ],
    'padding-block': [],
    'padding-block-end': [],
    'padding-block-start': [],
    'padding-bottom': [
        'auto'
    ],
    'padding-inline': [],
    'padding-inline-end': [],
    'padding-inline-start': [],
    'padding-left': [
        'auto'
    ],
    'padding-right': [
        'auto'
    ],
    'padding-top': [
        'auto'
    ],
    'page-break-after': [
        'auto',
        'always',
        'avoid',
        'left',
        'right'
    ],
    'page-break-before': [
        'auto',
        'always',
        'avoid',
        'left',
        'right'
    ],
    'page-break-inside': [
        'avoid',
        'auto'
    ],
    'paint-order': [
        'normal',
        'fill',
        'stroke',
        'markers'
    ],
    'perspective': [
        'none'
    ],
    'perspective-origin': [
        '<position>'
    ],
    'place-content': [],
    'place-items': [],
    'place-self': [],
    'pointer-events': [
        'auto',
        'none',
        'all',
        'visiblePainted',
        'visibleFill',
        'visibleStroke',
        'visible',
        'painted',
        'fill',
        'stroke'
    ],
    'position': [
        'static',
        'relative',
        'absolute',
        'fixed',
        'sticky'
    ],
    'prefix': [],
    'quotes': [
        'none',
        '<string>'
    ],
    'range': [
        'auto',
        'infinite'
    ],
    'resize': [
        'none',
        'both',
        'horizontal',
        'vertical'
    ],
    'right': [
        'auto'
    ],
    'rotate': [
        'none'
    ],
    'row-gap': [
        'normal'
    ],
    'ruby-align': [
        'auto',
        'center',
        'distribute-letter',
        'distribute-space',
        'left',
        'line-edge',
        'right',
        'space-around',
        'space-between',
        'start'
    ],
    'ruby-merge': [
        'auto',
        'collapse',
        'separate'
    ],
    'ruby-overhang': [
        'auto',
        'end',
        'none',
        'start'
    ],
    'ruby-position': [
        'after',
        'before',
        'inline',
        'inter-character',
        'over',
        'right',
        'under'
    ],
    'ruby-span': [
        ['attr()', 'attr($1)'],
        'none'
    ],
    'scale': [
        'none'
    ],
    'scroll-behavior': [
        'auto',
        'smooth'
    ],
    'scroll-margin': [],
    'scroll-margin-block': [],
    'scroll-margin-block-end': [],
    'scroll-margin-block-start': [],
    'scroll-margin-bottom': [],
    'scroll-margin-inline': [],
    'scroll-margin-inline-end': [],
    'scroll-margin-inline-start': [],
    'scroll-margin-left': [],
    'scroll-margin-right': [],
    'scroll-margin-top': [],
    'scroll-padding': [
        'auto'
    ],
    'scroll-padding-block': [
        'auto'
    ],
    'scroll-padding-block-end': [
        'auto'
    ],
    'scroll-padding-block-start': [
        'auto'
    ],
    'scroll-padding-bottom': [
        'auto'
    ],
    'scroll-padding-inline': [
        'auto'
    ],
    'scroll-padding-inline-end': [
        'auto'
    ],
    'scroll-padding-inline-start': [
        'auto'
    ],
    'scroll-padding-left': [
        'auto'
    ],
    'scroll-padding-right': [
        'auto'
    ],
    'scroll-padding-top': [
        'auto'
    ],
    'scroll-snap-align': [
        'center',
        'end',
        'none',
        'start'
    ],
    'scroll-snap-coordinate': [
        'none'
    ],
    'scroll-snap-destination': [],
    'scroll-snap-points-x': [
        ['repeat()', 'repeat($1)'],
        'none'
    ],
    'scroll-snap-points-y': [
        ['repeat()', 'repeat($1)'],
        'none'
    ],
    'scroll-snap-stop': [
        'always',
        'normal'
    ],
    'scroll-snap-type': [
        'block',
        'both',
        'inline',
        'mandatory',
        'none',
        'proximity'
    ],
    'scroll-snap-type-x': [
        'mandatory',
        'none',
        'proximity'
    ],
    'scroll-snap-type-y': [
        'mandatory',
        'none',
        'proximity'
    ],
    'scrollbar-3dlight-color': [],
    'scrollbar-arrow-color': [],
    'scrollbar-base-color': [],
    'scrollbar-color': [
        'auto',
        'dark',
        'light'
    ],
    'scrollbar-darkshadow-color': [],
    'scrollbar-face-color': [],
    'scrollbar-gutter': [
        'always',
        'auto',
        'stable'
    ],
    'scrollbar-highlight-color': [],
    'scrollbar-shadow-color': [],
    'scrollbar-track-color': [],
    'scrollbar-width': [
        'auto',
        'none',
        'thin'
    ],
    'shape-image-threshold': [],
    'shape-margin': [],
    'shape-outside': [
        'none',
        'margin-box',
        'content-box',
        'border-box',
        'padding-box',
        '<basic_shape>',
        '<uri>'
    ],
    'shape-rendering': [
        'auto',
        'optimizeSpeed',
        'crispEdges',
        'geometricPrecision'
    ],
    'size': [
        'a3',
        'a4',
        'a5',
        'b4',
        'b5',
        'jis-b4',
        'jis-b5',
        'landscape',
        'ledger',
        'legal',
        'letter',
        'portrait'
    ],
    'speak-as': [
        'auto',
        'bullets',
        'numbers',
        'spell-out',
        'words'
    ],
    'src': [
        ['format()', 'format($1)'],
        ['local()', 'local($1)'],
        '<uri>'
    ],
    'stop-color': [
        '<color>'
    ],
    'stop-opacity': [],
    'stroke': [
        '<color>'
    ],
    'stroke-dasharray': [
        'none'
    ],
    'stroke-dashoffset': [],
    'stroke-linecap': [
        'butt',
        'round',
        'square'
    ],
    'stroke-linejoin': [
        'round',
        'miter',
        'bevel'
    ],
    'stroke-miterlimit': [],
    'stroke-opacity': [],
    'stroke-width': [],
    'suffix': [],
    'symbols': [],
    'syntax': [],
    'system': [
        '<counter_symbols>'
    ],
    'tab-size': [],
    'table-layout': [
        'auto',
        'fixed'
    ],
    'text-align': [
        'left',
        'right',
        'center',
        'justify',
        'justify-all'
    ],
    'text-align-last': [
        'start',
        'end',
        'left',
        'right',
        'center',
        'justify'
    ],
    'text-anchor': [
        'start',
        'middle',
        'end'
    ],
    'text-combine-upright': [
        'all',
        'digits',
        'none'
    ],
    'text-decoration': [
        'none',
        'underline',
        'overline',
        'line-through',
        'blink'
    ],
    'text-decoration-color': [
        '<color>'
    ],
    'text-decoration-line': [
        'none',
        'underline',
        'overline',
        'line-through'
    ],
    'text-decoration-skip': [
        'box-decoration',
        'edges',
        'leading-spaces',
        'none',
        'objects',
        'spaces',
        'trailing-spaces'
    ],
    'text-decoration-skip-ink': [
        'all',
        'auto',
        'none'
    ],
    'text-decoration-style': [
        'solid',
        'double',
        'dotted',
        'dashed',
        'wavy'
    ],
    'text-decoration-thickness': [
        'auto',
        'from-font'
    ],
    'text-emphasis': [],
    'text-emphasis-color': [],
    'text-emphasis-position': [
        'left',
        'over',
        'right',
        'under'
    ],
    'text-emphasis-style': [
        'circle',
        'dot',
        'double-circle',
        'filled',
        'none',
        'open',
        'sesame',
        'triangle'
    ],
    'text-indent': [],
    'text-justify': [
        'auto',
        'distribute',
        'distribute-all-lines',
        'inter-character',
        'inter-cluster',
        'inter-ideograph',
        'inter-word',
        'kashida',
        'newspaper',
        'none'
    ],
    'text-orientation': [
        'mixed',
        'upright',
        'sideways',
        'use-glyph-orientation'
    ],
    'text-overflow': [
        'clip',
        'ellipsis'
    ],
    'text-rendering': [
        'auto',
        'optimizeSpeed',
        'optimizeLegibility',
        'geometricPrecision'
    ],
    'text-shadow': [
        'none',
        '<color>'
    ],
    'text-size-adjust': [
        'auto',
        'none'
    ],
    'text-transform': [
        'capitalize',
        'uppercase',
        'lowercase',
        'none'
    ],
    'text-underline-offset': [
        'auto'
    ],
    'text-underline-position': [
        'auto',
        'under',
        'left',
        'right'
    ],
    'top': [
        'auto'
    ],
    'touch-action': [
        'auto',
        'cross-slide-x',
        'cross-slide-y',
        'double-tap-zoom',
        'manipulation',
        'none',
        'pan-down',
        'pan-left',
        'pan-right',
        'pan-up',
        'pan-x',
        'pan-y',
        'pinch-zoom'
    ],
    'transform': [
        'none',
        ['matrix()', 'matrix(${1:1}, ${2:1}, ${3:1}, ${4:1}, ${5:2}, ${6:2})'],
        ['matrix3d()', 'matrix3d(${1:1}, ${2:1}, ${3:0}, ${4:0}, ${5:1}, ${6:1}, ${7:0}, ${8:0}, ${9:0}, ${10:0}, ${11:1}, ${12:0}, ${13:2}, ${14:2}, ${15:0}, ${16:1})'],
        ['perspective()', 'perspective(${1:0})'],
        ['rotate()', 'rotate(${1:45deg})'],
        ['rotate3d()', 'rotate3d(${1:0}, ${2:0}, ${3:1}, ${4:45deg})'],
        ['rotateX()', 'rotateX(${1:45deg})'],
        ['rotateY()', 'rotateY(${1:45deg})'],
        ['rotateZ()', 'rotateZ(${1:45deg})'],
        ['scale()', 'scale(${1:1.0})'],
        ['scale3d()', 'scale3d(${1:1.0}, ${2:1.0}, ${3:1.0})'],
        ['scaleX()', 'scaleX(${1:1.0})'],
        ['scaleY()', 'scaleY(${1:1.0})'],
        ['scaleZ()', 'scaleZ(${1:1.0})'],
        ['skew()', 'skew(${1:10deg})'],
        ['skewX()', 'skewX(${1:10deg})'],
        ['skewY()', 'skewY(${1:10deg})'],
        ['translate()', 'translate(${1:10px})'],
        ['translate3d()', 'translate3d(${1:10px}, ${2:0px}, ${3:0px})'],
        ['translateX()', 'translateX(${1:10px})'],
        ['translateY()', 'translateY(${1:10px})'],
        ['translateZ()', 'translateZ(${1:10px})']
    ],
    'transform-box': [
        'border-box',
        'content-box',
        'fill-box',
        'stroke-box',
        'view-box'
    ],
    'transform-origin': [
        '<position>'
    ],
    'transform-style': [
        'preserve-3d',
        'flat'
    ],
    'transition': [],
    'transition-delay': [],
    'transition-duration': [],
    'transition-property': [
        'none',
    ],
    'transition-timing-function': [
        '<timing>'
    ],
    'translate': [
        'none'
    ],
    'unicode-bidi': [
        'normal',
        'embed',
        'bidi-override'
    ],
    'unicode-range': [],
    'user-select': [
        'auto',
        'text',
        'none',
        'contain'
    ],
    'user-zoom': [
        'fixed',
        'zoom'
    ],
    'vertical-align': [
        'baseline',
        'sub',
        'super',
        'text-top',
        'text-bottom',
        'middle',
        'top',
        'bottom',
    ],
    'viewport-fit': [
        'auto',
        'contain',
        'cover'
    ],
    'visibility': [
        'visible',
        'hidden',
        'collapse'
    ],
    'white-space': [
        'normal',
        'pre',
        'nowrap',
        'pre-wrap',
        'pre-line'
    ],
    'widows': [],
    'width': [
        'auto',
        'fit-content'
    ],
    'will-change': [
        'auto',
        'contents',
        'scroll-position',
    ],
    'word-break': [
        'normal',
        'break-all',
        'keep-all'
    ],
    'word-spacing': [
        'normal',
    ],
    'word-wrap': [
        'normal',
        'break-word'
    ],
    'writing-mode': [
        'horizontal-tb',
        'vertical-rl',
        'vertical-lr',
        'sideways-rl',
        'sideways-lr'
    ],
    'z-index': [
        'auto',
    ],
    'zoom': [
        'auto',
        'normal'
    ]
}

add_values = [
    'inherit', 
    'initial', 
    'unset', 
    'var()'
    ]

def get_data(name):
    #get list of properties
    if not name:
        return list(properties_dict.keys())
    
    #get list of values for property
    r = []
    values = properties_dict.get(name, [])
    for val in values:
        if type(val) is str:
            if val.startswith('<') and val.endswith('>'):
                more = common_values.get(val[1:-1], [])
                for m in more:
                    if type(m) is str:
                        r.append(m)
                    elif type(m) is list:
                        r.append(m[0])
            else:
                r.append(val)
        elif type(val) is list:
            r.append(val[0])
    r += add_values
    return r
