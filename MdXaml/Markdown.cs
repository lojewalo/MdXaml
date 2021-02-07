using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;

namespace MdXaml {
    public class Markdown : DependencyObject, IUriContext {
        #region const

        /// <summary>
        /// maximum nested depth of [] and () supported by the transform; implementation detail
        /// </summary>
        private const int NestDepth = 6;

        private const string TagCode = "CodeSpan";
        private const string TagBoldSpan = "Bold";
        private const string TagItalicSpan = "Italic";
        private const string TagStrikethroughSpan = "Strikethrough";
        private const string TagUnderlineSpan = "Underline";

        #endregion

        /// <summary>
        /// when true, bold and italic require non-word characters on either side
        /// WARNING: this is a significant deviation from the markdown spec
        /// </summary>
        public bool StrictBoldItalic { get; set; }

        public bool DisabledTag { get; set; }

        public bool DisabledTootip { get; set; }

        public string AssetPathRoot { get; set; }

        public ICommand HyperlinkCommand { get; set; }

        public Uri BaseUri { get; set; }

        #region dependencyobject property

        // Using a DependencyProperty as the backing store for DocumentStyle.  This enables animation, styling, binding, etc...
        public static readonly DependencyProperty DocumentStyleProperty =
            DependencyProperty.Register(nameof(DocumentStyle), typeof(Style), typeof(Markdown), new PropertyMetadata(null));

        /// <summary>
        /// top-level flow document style
        /// </summary>
        public Style DocumentStyle {
            get => (Style) this.GetValue(DocumentStyleProperty);
            set => this.SetValue(DocumentStyleProperty, value);
        }

        #endregion

        #region legacy property

        public Style CodeStyle { get; set; }
        public Style LinkStyle { get; set; }

        #endregion

        #region regex pattern

        #endregion

        public Markdown() {
            this.HyperlinkCommand = NavigationCommands.GoToPage;
            this.AssetPathRoot = Environment.CurrentDirectory;
        }

        /// <summary>
        /// Perform transformations that occur *within* block-level tags like paragraphs, headers, and list items.
        /// </summary>
        public IEnumerable<Inline> RunSpanGamut(string text) {
            if (text is null) {
                throw new ArgumentNullException(nameof(text));
            }

            return this.DoCodeSpans(text,
                s0 => this.DoImagesOrHrefs(s0,
                    s1 => this.DoTextDecorations(s1,
                        DoText)));
        }

        #region grammar - image or href

        private static readonly Regex ImageOrHrefInline = new($@"
                (                           # wrap whole match in $1
                    (!)?                    # image maker = $2
                    \[
                        ({GetNestedBracketsPattern()})               # link text = $3
                    \]
                    \(                      # literal paren
                        [ ]*
                        ({GetNestedParensPattern()})               # href = $4
                        [ ]*
                        (                   # $5
                        (['""])             # quote char = $6
                        (.*?)               # title = $7
                        \6                  # matching quote
                        [ ]*                # ignore any spaces between closing quote and )
                        )?                  # title is optional
                    \)
                )",
            RegexOptions.Singleline | RegexOptions.IgnorePatternWhitespace | RegexOptions.Compiled);

        private IEnumerable<Inline> DoImagesOrHrefs(string text, Func<string, IEnumerable<Inline>> defaultHandler) {
            if (text is null) {
                throw new ArgumentNullException(nameof(text));
            }

            return Evaluate(text, ImageOrHrefInline, this.ImageOrHrefInlineEvaluator, defaultHandler);
        }

        private Inline ImageOrHrefInlineEvaluator(Match match) {
            if (match is null) {
                throw new ArgumentNullException(nameof(match));
            }

            return string.IsNullOrEmpty(match.Groups[2].Value) ? this.TreatsAsHref(match) : TreatsAsImage(match);
        }

        private Inline TreatsAsHref(Match match) {
            if (match is null) {
                throw new ArgumentNullException(nameof(match));
            }

            var linkText = match.Groups[3].Value;
            var url = match.Groups[4].Value;
            var title = match.Groups[7].Value;

            var result = Create<Hyperlink, Inline>(this.RunSpanGamut(linkText));
            result.Command = this.HyperlinkCommand;
            result.CommandParameter = url;

            if (!this.DisabledTootip) {
                result.ToolTip = string.IsNullOrWhiteSpace(title) ? url : $"\"{title}\"\r\n{url}";
            }

            if (this.LinkStyle != null) {
                result.Style = this.LinkStyle;
            }

            return result;
        }

        private static Inline TreatsAsImage(Match match) {
            return new Run(match.Groups[0].Value);
        }

        #endregion

        #region grammar - code

        private static readonly Regex CodeSpan = new(@"
                    (?<!\\)   # Character before opening ` can't be a backslash
                    (`+)      # $1 = Opening run of `
                    (.+?)     # $2 = The code block
                    (?<!`)
                    \1
                    (?!`)", RegexOptions.IgnorePatternWhitespace | RegexOptions.Singleline | RegexOptions.Compiled);

        /// <summary>
        /// Turn Markdown `code spans` into HTML code tags
        /// </summary>
        private IEnumerable<Inline> DoCodeSpans(string text, Func<string, IEnumerable<Inline>> defaultHandler) {
            if (text is null) {
                throw new ArgumentNullException(nameof(text));
            }

            //    * You can use multiple backticks as the delimiters if you want to
            //        include literal backticks in the code span. So, this input:
            //
            //        Just type ``foo `bar` baz`` at the prompt.
            //
            //        Will translate to:
            //
            //          <p>Just type <code>foo `bar` baz</code> at the prompt.</p>
            //
            //        There's no arbitrary limit to the number of backticks you
            //        can use as delimters. If you need three consecutive backticks
            //        in your code, use four for delimiters, etc.
            //
            //    * You can use spaces to get literal backticks at the edges:
            //
            //          ... type `` `bar` `` ...
            //
            //        Turns to:
            //
            //          ... type <code>`bar`</code> ...
            //

            return Evaluate(text, CodeSpan, this.CodeSpanEvaluator, defaultHandler);
        }

        private Inline CodeSpanEvaluator(Match match) {
            if (match is null) {
                throw new ArgumentNullException(nameof(match));
            }

            var span = match.Groups[2].Value;
            span = Regex.Replace(span, @"^[ ]*", ""); // leading whitespace
            span = Regex.Replace(span, @"[ ]*$", ""); // trailing whitespace

            var result = new Run(span);
            if (this.CodeStyle != null) {
                result.Style = this.CodeStyle;
            }

            if (!this.DisabledTag) {
                result.Tag = TagCode;
            }

            return result;
        }

        #endregion

        #region grammar - textdecorations

        private static readonly Regex StrictBold = new(@"([\W_]|^) (\*\*|__) (?=\S) ([^\r]*?\S[\*_]*) \2 ([\W_]|$)",
            RegexOptions.IgnorePatternWhitespace | RegexOptions.Singleline | RegexOptions.Compiled);

        private static readonly Regex StrictItalic = new(@"([\W_]|^) (\*|_) (?=\S) ([^\r\*_]*?\S) \2 ([\W_]|$)",
            RegexOptions.IgnorePatternWhitespace | RegexOptions.Singleline | RegexOptions.Compiled);

        private static readonly Regex Strikethrough = new(@"(~~) (?=\S) (.+?) (?<=\S) \1",
            RegexOptions.IgnorePatternWhitespace | RegexOptions.Singleline | RegexOptions.Compiled);

        private static readonly Regex Underline = new(@"(__) (?=\S) (.+?) (?<=\S) \1",
            RegexOptions.IgnorePatternWhitespace | RegexOptions.Singleline | RegexOptions.Compiled);

        private static readonly Regex Color = new(@"%\{[ \t]*color[ \t]*:([^\}]+)\}", RegexOptions.Compiled);

        /// <summary>
        /// Turn Markdown *italics* and **bold** into HTML strong and em tags
        /// </summary>
        private IEnumerable<Inline> DoTextDecorations(string text, Func<string, IEnumerable<Inline>> defaultHandler) {
            if (text is null) {
                throw new ArgumentNullException(nameof(text));
            }

            // <strong> must go first, then <em>
            if (this.StrictBoldItalic) {
                return Evaluate(text, StrictBold, m => this.BoldEvaluator(m, 3),
                    s1 => Evaluate(s1, StrictItalic, m => this.ItalicEvaluator(m, 3),
                        s2 => Evaluate(s2, Strikethrough, m => this.StrikethroughEvaluator(m, 2),
                            s3 => Evaluate(s3, Underline, m => this.UnderlineEvaluator(m, 2),
                                s4 => defaultHandler(s4)))));
            } else {
                var rtn = new List<Inline>();

                var buff = new StringBuilder();

                void HandleBefore() {
                    if (buff.Length <= 0) {
                        return;
                    }

                    rtn.AddRange(defaultHandler(buff.ToString()));
                    buff.Clear();
                }

                for (var i = 0; i < text.Length; ++i) {
                    var ch = text[i];
                    switch (ch) {
                        default:
                            buff.Append(ch);
                            break;

                        case '\\': // escape
                            if (++i < text.Length) {
                                switch (text[i]) {
                                    default:
                                        buff.Append('\\').Append(text[i]);
                                        break;

                                    case '\\': // escape
                                    case ':': // bold? or italic
                                    case '*': // bold? or italic
                                    case '~': // strikethrough?
                                    case '_': // underline?
                                    case '%': // color?
                                        buff.Append(text[i]);
                                        break;
                                }
                            } else {
                                buff.Append('\\');
                            }

                            break;

                        case ':': // emoji?
                        {
                            var nxtI = text.IndexOf(':', i + 1);
                            if (nxtI != -1 && EmojiTable.TryGet(text.Substring(i + 1, nxtI - i - 1), out var emoji)) {
                                buff.Append(emoji);
                                i = nxtI;
                            } else {
                                buff.Append(':');
                            }

                            break;
                        }

                        case '*': // bold? or italic
                        {
                            var oldI = i;
                            var inline = this.ParseAsBoldOrItalic(text, ref i);
                            if (inline == null) {
                                buff.Append(text, oldI, i - oldI + 1);
                            } else {
                                HandleBefore();
                                rtn.Add(inline);
                            }

                            break;
                        }

                        case '~': // strikethrough?
                        {
                            var oldI = i;
                            var inline = this.ParseAsStrikethrough(text, ref i);
                            if (inline == null) {
                                buff.Append(text, oldI, i - oldI + 1);
                            } else {
                                HandleBefore();
                                rtn.Add(inline);
                            }

                            break;
                        }

                        case '_': // underline?
                        {
                            var oldI = i;
                            var inline = this.ParseAsUnderline(text, ref i);
                            if (inline == null) {
                                buff.Append(text, oldI, i - oldI + 1);
                            } else {
                                HandleBefore();
                                rtn.Add(inline);
                            }

                            break;
                        }

                        case '%': // color?
                        {
                            var oldI = i;
                            var inline = this.ParseAsColor(text, ref i);
                            if (inline == null) {
                                buff.Append(text, oldI, i - oldI + 1);
                            } else {
                                HandleBefore();
                                rtn.Add(inline);
                            }

                            break;
                        }
                    }
                }

                if (buff.Length > 0) {
                    rtn.AddRange(defaultHandler(buff.ToString()));
                }

                return rtn;
            }
        }

        private Inline ParseAsUnderline(string text, ref int start) {
            var bgnCnt = CountRepeat(text, start, '_');

            var last = EscapedIndexOf(text, start + bgnCnt, '_');

            var endCnt = last >= 0 ? CountRepeat(text, last, '_') : -1;

            if (endCnt >= 2 && bgnCnt >= 2) {
                const int cnt = 2;
                var bgn = start + cnt;
                var end = last;

                start = end + cnt - 1;
                var span = Create<Underline, Inline>(this.RunSpanGamut(text.Substring(bgn, end - bgn)));
                if (!this.DisabledTag) {
                    span.Tag = TagUnderlineSpan;
                }

                return span;
            } else {
                start += bgnCnt - 1;
                return null;
            }
        }

        private Inline ParseAsStrikethrough(string text, ref int start) {
            var bgnCnt = CountRepeat(text, start, '~');

            var last = EscapedIndexOf(text, start + bgnCnt, '~');

            var endCnt = last >= 0 ? CountRepeat(text, last, '~') : -1;

            if (endCnt >= 2 && bgnCnt >= 2) {
                const int cnt = 2;
                var bgn = start + cnt;
                var end = last;

                start = end + cnt - 1;
                var span = Create<Span, Inline>(this.RunSpanGamut(text.Substring(bgn, end - bgn)));
                span.TextDecorations = TextDecorations.Strikethrough;

                if (!this.DisabledTag) {
                    span.Tag = TagStrikethroughSpan;
                }

                return span;
            }

            start += bgnCnt - 1;
            return null;
        }

        private Inline ParseAsBoldOrItalic(string text, ref int start) {
            // count asterisk (bgn)
            var bgnCnt = CountRepeat(text, start, '*');

            var last = EscapedIndexOf(text, start + bgnCnt, '*');

            var endCnt = last >= 0 ? CountRepeat(text, last, '*') : -1;

            if (endCnt >= 1) {
                var cnt = Math.Min(bgnCnt, endCnt);
                var bgn = start + cnt;
                var end = last;

                switch (cnt) {
                    case 1: // italic
                    {
                        start = end + cnt - 1;

                        var span = Create<Italic, Inline>(this.RunSpanGamut(text.Substring(bgn, end - bgn)));
                        if (!this.DisabledTag) {
                            span.Tag = TagItalicSpan;
                        }

                        return span;
                    }
                    case 2: // bold
                    {
                        start = end + cnt - 1;
                        var span = Create<Bold, Inline>(this.RunSpanGamut(text.Substring(bgn, end - bgn)));
                        if (!this.DisabledTag) {
                            span.Tag = TagBoldSpan;
                        }

                        return span;
                    }

                    default: // >3; bold-italic
                    {
                        bgn = start + 3;
                        start = end + 3 - 1;

                        var inline = Create<Italic, Inline>(this.RunSpanGamut(text.Substring(bgn, end - bgn)));
                        if (!this.DisabledTag) {
                            inline.Tag = TagItalicSpan;
                        }

                        var span = new Bold(inline);
                        if (!this.DisabledTag) {
                            span.Tag = TagBoldSpan;
                        }

                        return span;
                    }
                }
            }

            start += bgnCnt - 1;
            return null;
        }

        private Inline ParseAsColor(string text, ref int start) {
            var mch = Color.Match(text, start);

            if (!mch.Success || start != mch.Index) {
                return null;
            }

            var bgnIdx = start + mch.Value.Length;
            var endIdx = EscapedIndexOf(text, bgnIdx, '%');

            Span span;
            if (endIdx == -1) {
                endIdx = text.Length - 1;
                span = Create<Span, Inline>(this.RunSpanGamut(text.Substring(bgnIdx)));
            } else {
                span = Create<Span, Inline>(this.RunSpanGamut(text.Substring(bgnIdx, endIdx - bgnIdx)));
            }

            var colorLbl = mch.Groups[1].Value;

            try {
                var color = colorLbl.StartsWith("#") ? (SolidColorBrush) new BrushConverter().ConvertFrom(colorLbl) : (SolidColorBrush) new BrushConverter().ConvertFromString(colorLbl);

                span.Foreground = color;
            } catch {
                // ignored
            }

            start = endIdx;
            return span;

        }


        private static int EscapedIndexOf(string text, int start, char target) {
            for (var i = start; i < text.Length; ++i) {
                var ch = text[i];
                if (ch == '\\') {
                    ++i;
                } else if (ch == target) {
                    return i;
                }
            }

            return -1;
        }

        private static int CountRepeat(string text, int start, char target) {
            var count = 0;

            for (var i = start; i < text.Length; ++i) {
                if (text[i] == target) {
                    ++count;
                } else {
                    break;
                }
            }

            return count;
        }


        private Inline ItalicEvaluator(Match match, int contentGroup) {
            if (match is null) {
                throw new ArgumentNullException(nameof(match));
            }

            var content = match.Groups[contentGroup].Value;
            var span = Create<Italic, Inline>(this.RunSpanGamut(content));
            if (!this.DisabledTag) {
                span.Tag = TagItalicSpan;
            }

            return span;
        }

        private Inline BoldEvaluator(Match match, int contentGroup) {
            if (match is null) {
                throw new ArgumentNullException(nameof(match));
            }

            var content = match.Groups[contentGroup].Value;
            var span = Create<Bold, Inline>(this.RunSpanGamut(content));
            if (!this.DisabledTag) {
                span.Tag = TagBoldSpan;
            }

            return span;
        }

        private Inline StrikethroughEvaluator(Match match, int contentGroup) {
            if (match is null) {
                throw new ArgumentNullException(nameof(match));
            }

            var content = match.Groups[contentGroup].Value;

            var span = Create<Span, Inline>(this.RunSpanGamut(content));
            span.TextDecorations = TextDecorations.Strikethrough;
            if (!this.DisabledTag) {
                span.Tag = TagStrikethroughSpan;
            }

            return span;
        }

        private Inline UnderlineEvaluator(Match match, int contentGroup) {
            if (match is null) {
                throw new ArgumentNullException(nameof(match));
            }

            var content = match.Groups[contentGroup].Value;
            var span = Create<Underline, Inline>(this.RunSpanGamut(content));
            if (!this.DisabledTag) {
                span.Tag = TagUnderlineSpan;
            }

            return span;
        }

        #endregion

        #region grammar - text

        private static readonly Regex Eoln = new("\\s+");
        private static readonly Regex Lbrk = new(@"\ {2,}\n");

        public static IEnumerable<Inline> DoText(string text) {
            if (text is null) {
                throw new ArgumentNullException(nameof(text));
            }

            var lines = Lbrk.Split(text);
            var first = true;
            foreach (var line in lines) {
                if (first) {
                    first = false;
                } else {
                    yield return new LineBreak();
                }

                var t = Eoln.Replace(line, " ");
                yield return new Run(t);
            }
        }

        #endregion

        #region helper - make regex

        private static string _nestedBracketsPattern;

        /// <summary>
        /// Reusable pattern to match balanced [brackets]. See Friedl's
        /// "Mastering Regular Expressions", 2nd Ed., pp. 328-331.
        /// </summary>
        private static string GetNestedBracketsPattern() {
            // in other words [this] and [this[also]] and [this[also[too]]]
            // up to _nestDepth
            return _nestedBracketsPattern ??= RepeatString(@"
                    (?>              # Atomic matching
                       [^()\n\t]+? # Anything other than parens or whitespace
                     |
                       \[
                           ", NestDepth) + RepeatString(
                @" \]
                    )*", NestDepth);
        }

        private static string _nestedParensPattern;

        /// <summary>
        /// Reusable pattern to match balanced (parens). See Friedl's
        /// "Mastering Regular Expressions", 2nd Ed., pp. 328-331.
        /// </summary>
        private static string GetNestedParensPattern() {
            // in other words (this) and (this(also)) and (this(also(too)))
            // up to _nestDepth
            return _nestedParensPattern ??= RepeatString(@"
                    (?>              # Atomic matching
                       [^()\r\n\t]+?      # Anything other than parens or linebreak
                     |
                       \(
                           ", NestDepth) + RepeatString(
                @" \)
                    )*?", NestDepth);
        }

        /// <summary>
        /// this is to emulate what's evailable in PHP
        /// </summary>
        private static string RepeatString(string text, int count) {
            if (text is null) {
                throw new ArgumentNullException(nameof(text));
            }

            var sb = new StringBuilder(text.Length * count);
            for (var i = 0; i < count; i++) {
                sb.Append(text);
            }

            return sb.ToString();
        }

        #endregion

        #region helper - parse

        private static TResult Create<TResult, TContent>(IEnumerable<TContent> content)
            where TResult : IAddChild, new() {
            var result = new TResult();
            foreach (var c in content) {
                result.AddChild(c);
            }

            return result;
        }

        private static IEnumerable<T> Evaluate<T>(string text, Regex expression, Func<Match, T> build, Func<string, IEnumerable<T>> rest) {
            if (text is null) {
                throw new ArgumentNullException(nameof(text));
            }

            var matches = expression.Matches(text);
            var index = 0;
            foreach (Match m in matches) {
                if (m.Index > index) {
                    var prefix = text.Substring(index, m.Index - index);
                    foreach (var t in rest(prefix)) {
                        yield return t;
                    }
                }

                yield return build(m);

                index = m.Index + m.Length;
            }

            if (index >= text.Length) {
                yield break;
            }

            {
                var suffix = text.Substring(index, text.Length - index);
                foreach (var t in rest(suffix)) {
                    yield return t;
                }
            }
        }

        #endregion
    }
}
