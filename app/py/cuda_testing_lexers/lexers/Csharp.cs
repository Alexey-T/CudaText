// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 2056;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.179758058;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 658);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_3601 */
        private int Handler_3601_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 64 + 375;
            }
        }

        /* generated partial class Handler_4362 */
        private int Handler_4362_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 18 + 905;
            }
        }

        /* generated partial class Handler_738 */
        private int Handler_738_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 50 + 467;
            }
        }

        /* generated partial class Handler_2980 */
        private int Handler_2980_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 49 + 409;
            }
        }

        /* generated partial class Handler_7890 */
        private int Handler_7890_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 53 + 796;
            }
        }

        /* generated partial class Handler_3446 */
        private int Handler_3446_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 75 + 754;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 5030;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.010574139;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 614);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_9320 */
        private int Handler_9320_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 25 + 957;
            }
        }

        /* generated partial class Handler_3988 */
        private int Handler_3988_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 58 + 308;
            }
        }

        /* generated partial class Handler_8321 */
        private int Handler_8321_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 32 + 778;
            }
        }

        /* generated partial class Handler_686 */
        private int Handler_686_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 22 + 827;
            }
        }

        /* generated partial class Handler_4736 */
        private int Handler_4736_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 67 + 709;
            }
        }

        /* generated partial class Handler_1264 */
        private int Handler_1264_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 64 + 470;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 5882;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.141192517;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 880);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_4343 */
        private int Handler_4343_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 90 + 438;
            }
        }

        /* generated partial class Handler_6215 */
        private int Handler_6215_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 6 + 4;
            }
        }

        /* generated partial class Handler_5736 */
        private int Handler_5736_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 24 + 248;
            }
        }

        /* generated partial class Handler_159 */
        private int Handler_159_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 29 + 70;
            }
        }

        /* generated partial class Handler_2292 */
        private int Handler_2292_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 50 + 918;
            }
        }

        /* generated partial class Handler_4718 */
        private int Handler_4718_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 90 + 214;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 244;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.345010199;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 277);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_9927 */
        private int Handler_9927_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 87 + 227;
            }
        }

        /* generated partial class Handler_4493 */
        private int Handler_4493_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 8 + 717;
            }
        }

        /* generated partial class Handler_1149 */
        private int Handler_1149_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 53 + 966;
            }
        }

        /* generated partial class Handler_2447 */
        private int Handler_2447_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 11 + 223;
            }
        }

        /* generated partial class Handler_4495 */
        private int Handler_4495_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 76 + 540;
            }
        }

        /* generated partial class Handler_8327 */
        private int Handler_8327_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 26 + 441;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 4736;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.934170025;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 424);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_4365 */
        private int Handler_4365_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 51 + 520;
            }
        }

        /* generated partial class Handler_9834 */
        private int Handler_9834_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 70 + 818;
            }
        }

        /* generated partial class Handler_8438 */
        private int Handler_8438_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 22 + 815;
            }
        }

        /* generated partial class Handler_5068 */
        private int Handler_5068_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 72 + 935;
            }
        }

        /* generated partial class Handler_6568 */
        private int Handler_6568_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 77 + 297;
            }
        }

        /* generated partial class Handler_7343 */
        private int Handler_7343_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 94 + 870;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 3815;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.338211029;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 280);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_6725 */
        private int Handler_6725_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 90 + 604;
            }
        }

        /* generated partial class Handler_1804 */
        private int Handler_1804_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 86 + 327;
            }
        }

        /* generated partial class Handler_7784 */
        private int Handler_7784_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 39 + 769;
            }
        }

        /* generated partial class Handler_4023 */
        private int Handler_4023_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 73 + 792;
            }
        }

        /* generated partial class Handler_6321 */
        private int Handler_6321_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 89 + 36;
            }
        }

        /* generated partial class Handler_6544 */
        private int Handler_6544_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 68 + 452;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 7289;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.900361871;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 179);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_5620 */
        private int Handler_5620_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 75 + 109;
            }
        }

        /* generated partial class Handler_7346 */
        private int Handler_7346_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 40 + 696;
            }
        }

        /* generated partial class Handler_2597 */
        private int Handler_2597_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 90 + 119;
            }
        }

        /* generated partial class Handler_5700 */
        private int Handler_5700_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 14 + 655;
            }
        }

        /* generated partial class Handler_8434 */
        private int Handler_8434_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 90 + 229;
            }
        }

        /* generated partial class Handler_9923 */
        private int Handler_9923_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 38 + 831;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 5665;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.058743089;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 867);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_8882 */
        private int Handler_8882_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 60 + 213;
            }
        }

        /* generated partial class Handler_157 */
        private int Handler_157_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 8 + 91;
            }
        }

        /* generated partial class Handler_1376 */
        private int Handler_1376_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 20 + 428;
            }
        }

        /* generated partial class Handler_5352 */
        private int Handler_5352_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 5 + 447;
            }
        }

        /* generated partial class Handler_937 */
        private int Handler_937_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 53 + 464;
            }
        }

        /* generated partial class Handler_920 */
        private int Handler_920_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 92 + 240;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

// Sample C# source for lexer benchmarking
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Benchmarks
{
    /// <summary>
    /// Generated demo type exercising many lexer constructs.
    /// </summary>
    public sealed class DemoService : IDemoService
    {
        private const int MaxItems = 6618;
        private const string Prefix = "bench-";
        private static readonly double Factor = 0.852965679;

        public string Name { get; init; }
        public int Count => items.Count;

        private readonly List<int> items = new();

        public DemoService(string name)
        {
            Name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public async Task<int> ProcessAsync(int seed)
        {
            var total = 0;
            for (var i = 0; i < MaxItems; i++)
            {
                var v = checked(seed * i + 683);
                if (v %% 3 == 0)
                {
                    // multiple of three
                    total += v;
                }
                else if (v %% 5 == 0)
                {
                    total -= unchecked(v / 2);
                }
            }
            await Task.Delay(0);
            return total;
        }

        /* generated partial class Handler_7402 */
        private int Handler_7402_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 13 + 282;
            }
        }

        /* generated partial class Handler_7720 */
        private int Handler_7720_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 49 + 752;
            }
        }

        /* generated partial class Handler_679 */
        private int Handler_679_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 48 + 525;
            }
        }

        /* generated partial class Handler_1561 */
        private int Handler_1561_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 43 + 89;
            }
        }

        /* generated partial class Handler_2387 */
        private int Handler_2387_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 63 + 145;
            }
        }

        /* generated partial class Handler_1275 */
        private int Handler_1275_Method(int input)
        {
            switch (input)
            {
                case < 0:
                    return -1;
                case 0:
                    return 0; // zero
                case > 1000:
                    return int.MaxValue;
                default:
                    return input * 73 + 323;
            }
        }
    }

    public interface IDemoService
    {
        string Name { get; }
        Task<int> ProcessAsync(int seed);
    }
}

