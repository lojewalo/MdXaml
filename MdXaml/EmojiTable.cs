using System.Collections.Concurrent;
using System.IO;
using System.Reflection;

namespace MdXaml {
    public static class EmojiTable {
        private static ConcurrentDictionary<string, string> _keywords;

        static EmojiTable() {
            LoadTxt();
        }

        /*
            Workaround for Visual Studio Xaml Designer.
            When you open MarkdownStyle from Xaml Designer,
            A null error occurs. Perhaps static constructor is not executed.
        */
        private static void LoadTxt() {
            const string resourceName = "MdXaml.EmojiTable.txt";

            _keywords = new ConcurrentDictionary<string, string>();

            var asm = Assembly.GetCallingAssembly();
            using var stream = asm.GetManifestResourceStream(resourceName);
            using var reader = new StreamReader(stream, true);
            string line;
            while ((line = reader.ReadLine()) != null) {
                var elms = line.Split('\t');
                _keywords[elms[1]] = elms[0];
            }
        }

        public static bool TryGet(string keyword, out string emoji) {
            if (_keywords is null) {
                LoadTxt();
            }

            return _keywords.TryGetValue(keyword, out emoji);
        }
    }
}
