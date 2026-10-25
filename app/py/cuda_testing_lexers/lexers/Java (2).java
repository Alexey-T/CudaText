// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 7029;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.048624865;

    private final Map<String, List<Integer>> index = new HashMap<>(19);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 566;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0x84;
            }
        }
        return total;
    }

    /** generated helper Task_4956 */
    private Optional<String> Task_4956(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_4956");
    }

    /** generated helper Task_4085 */
    private Optional<String> Task_4085(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_4085");
    }

    /** generated helper Task_6060 */
    private Optional<String> Task_6060(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_6060");
    }

    /** generated helper Task_2403 */
    private Optional<String> Task_2403(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_2403");
    }

    /** generated helper Task_8350 */
    private Optional<String> Task_8350(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8350");
    }

    /** generated helper Task_3439 */
    private Optional<String> Task_3439(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_3439");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 7771;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.280963364;

    private final Map<String, List<Integer>> index = new HashMap<>(152);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 342;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0xD3;
            }
        }
        return total;
    }

    /** generated helper Task_261 */
    private Optional<String> Task_261(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_261");
    }

    /** generated helper Task_1730 */
    private Optional<String> Task_1730(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_1730");
    }

    /** generated helper Task_2149 */
    private Optional<String> Task_2149(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_2149");
    }

    /** generated helper Task_5898 */
    private Optional<String> Task_5898(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_5898");
    }

    /** generated helper Task_3966 */
    private Optional<String> Task_3966(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_3966");
    }

    /** generated helper Task_6693 */
    private Optional<String> Task_6693(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_6693");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 5580;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.912701838;

    private final Map<String, List<Integer>> index = new HashMap<>(152);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 462;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0x6E;
            }
        }
        return total;
    }

    /** generated helper Task_5212 */
    private Optional<String> Task_5212(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_5212");
    }

    /** generated helper Task_3765 */
    private Optional<String> Task_3765(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_3765");
    }

    /** generated helper Task_7974 */
    private Optional<String> Task_7974(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_7974");
    }

    /** generated helper Task_2492 */
    private Optional<String> Task_2492(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_2492");
    }

    /** generated helper Task_9064 */
    private Optional<String> Task_9064(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_9064");
    }

    /** generated helper Task_9915 */
    private Optional<String> Task_9915(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_9915");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 7730;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.245745301;

    private final Map<String, List<Integer>> index = new HashMap<>(101);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 739;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0x2B;
            }
        }
        return total;
    }

    /** generated helper Task_8336 */
    private Optional<String> Task_8336(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8336");
    }

    /** generated helper Task_8536 */
    private Optional<String> Task_8536(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8536");
    }

    /** generated helper Task_4458 */
    private Optional<String> Task_4458(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_4458");
    }

    /** generated helper Task_515 */
    private Optional<String> Task_515(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_515");
    }

    /** generated helper Task_2704 */
    private Optional<String> Task_2704(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_2704");
    }

    /** generated helper Task_8464 */
    private Optional<String> Task_8464(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8464");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 1205;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.588896007;

    private final Map<String, List<Integer>> index = new HashMap<>(43);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 754;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0x43;
            }
        }
        return total;
    }

    /** generated helper Task_6044 */
    private Optional<String> Task_6044(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_6044");
    }

    /** generated helper Task_6777 */
    private Optional<String> Task_6777(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_6777");
    }

    /** generated helper Task_3917 */
    private Optional<String> Task_3917(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_3917");
    }

    /** generated helper Task_3299 */
    private Optional<String> Task_3299(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_3299");
    }

    /** generated helper Task_7276 */
    private Optional<String> Task_7276(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_7276");
    }

    /** generated helper Task_8509 */
    private Optional<String> Task_8509(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8509");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 2620;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.178172576;

    private final Map<String, List<Integer>> index = new HashMap<>(135);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 294;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0x07;
            }
        }
        return total;
    }

    /** generated helper Task_1296 */
    private Optional<String> Task_1296(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_1296");
    }

    /** generated helper Task_5572 */
    private Optional<String> Task_5572(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_5572");
    }

    /** generated helper Task_1518 */
    private Optional<String> Task_1518(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_1518");
    }

    /** generated helper Task_6175 */
    private Optional<String> Task_6175(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_6175");
    }

    /** generated helper Task_8620 */
    private Optional<String> Task_8620(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8620");
    }

    /** generated helper Task_5657 */
    private Optional<String> Task_5657(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_5657");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 2166;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.941204103;

    private final Map<String, List<Integer>> index = new HashMap<>(24);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 179;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0x33;
            }
        }
        return total;
    }

    /** generated helper Task_7419 */
    private Optional<String> Task_7419(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_7419");
    }

    /** generated helper Task_5439 */
    private Optional<String> Task_5439(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_5439");
    }

    /** generated helper Task_231 */
    private Optional<String> Task_231(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_231");
    }

    /** generated helper Task_7690 */
    private Optional<String> Task_7690(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_7690");
    }

    /** generated helper Task_9156 */
    private Optional<String> Task_9156(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_9156");
    }

    /** generated helper Task_29 */
    private Optional<String> Task_29(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_29");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 4024;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.237508741;

    private final Map<String, List<Integer>> index = new HashMap<>(68);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 980;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0xB1;
            }
        }
        return total;
    }

    /** generated helper Task_4349 */
    private Optional<String> Task_4349(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_4349");
    }

    /** generated helper Task_8917 */
    private Optional<String> Task_8917(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8917");
    }

    /** generated helper Task_8402 */
    private Optional<String> Task_8402(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8402");
    }

    /** generated helper Task_8875 */
    private Optional<String> Task_8875(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8875");
    }

    /** generated helper Task_191 */
    private Optional<String> Task_191(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_191");
    }

    /** generated helper Task_1386 */
    private Optional<String> Task_1386(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_1386");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 7810;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.851148629;

    private final Map<String, List<Integer>> index = new HashMap<>(178);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 764;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0x01;
            }
        }
        return total;
    }

    /** generated helper Task_788 */
    private Optional<String> Task_788(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_788");
    }

    /** generated helper Task_9650 */
    private Optional<String> Task_9650(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_9650");
    }

    /** generated helper Task_8686 */
    private Optional<String> Task_8686(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8686");
    }

    /** generated helper Task_9260 */
    private Optional<String> Task_9260(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_9260");
    }

    /** generated helper Task_6915 */
    private Optional<String> Task_6915(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_6915");
    }

    /** generated helper Task_8624 */
    private Optional<String> Task_8624(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_8624");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 5196;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.055595247;

    private final Map<String, List<Integer>> index = new HashMap<>(87);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 313;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0xEC;
            }
        }
        return total;
    }

    /** generated helper Task_6342 */
    private Optional<String> Task_6342(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_6342");
    }

    /** generated helper Task_3519 */
    private Optional<String> Task_3519(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_3519");
    }

    /** generated helper Task_7598 */
    private Optional<String> Task_7598(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_7598");
    }

    /** generated helper Task_3996 */
    private Optional<String> Task_3996(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_3996");
    }

    /** generated helper Task_9051 */
    private Optional<String> Task_9051(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_9051");
    }

    /** generated helper Task_949 */
    private Optional<String> Task_949(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_949");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

// Sample Java source for lexer benchmarking
package com.example.bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Demo service generated for lexer benchmarking.
 */
public final class DemoService implements AutoCloseable {

    private static final int MAX_ITEMS = 7048;
    private static final String PREFIX = "bench-";
    private static final double FACTOR = 0.012975223;

    private final Map<String, List<Integer>> index = new HashMap<>(19);

    public DemoService() {
        index.put("default", new ArrayList<>());
    }

    @Override
    public void close() {
        index.clear();
    }

    public synchronized int process(final int seed) {
        int total = 0;
        for (int i = 0; i < MAX_ITEMS; i++) {
            final int v = seed * i + 906;
            if ((v & 1) == 0) {
                total += v;
            } else if (v %% 3 == 0) {
                total -= v / 2;
            } else {
                /* keep */
                total ^= 0xB0;
            }
        }
        return total;
    }

    /** generated helper Task_4364 */
    private Optional<String> Task_4364(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_4364");
    }

    /** generated helper Task_5388 */
    private Optional<String> Task_5388(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_5388");
    }

    /** generated helper Task_9652 */
    private Optional<String> Task_9652(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_9652");
    }

    /** generated helper Task_1674 */
    private Optional<String> Task_1674(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_1674");
    }

    /** generated helper Task_4057 */
    private Optional<String> Task_4057(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_4057");
    }

    /** generated helper Task_638 */
    private Optional<String> Task_638(final long key)
    {
        if (key <= 0L) {
            return Optional.empty();
        }
        return Optional.of(PREFIX + Long.toHexString(key) + "-Task_638");
    }

    public static void main(final String[] args) {
        try (DemoService service = new DemoService()) {
            final int result = service.process(Integer.parseInt(args[0]));
            System.out.printf("result=%%d%%n", result);
        } catch (final Exception ex) {
            System.err.println("failed: " + ex.getMessage());
        }
    }
}

