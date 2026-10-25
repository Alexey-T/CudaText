/*
 * Sample C source generated for lexer benchmarking.
 * Exercises comments, strings, numbers, preprocessor, folding.
 */
#include <stdio.h>
#include <stdlib.h>
#include "local_header.h"

#define MAX_ITEMS 3376
#define PI_FLOAT 3.985998f
#ifdef _WIN32
#define PLATFORM_NAME "windows"
#else
#define PLATFORM_NAME "posix"
#endif

typedef struct demo0_s {
    int id;
    char name[64];
    double weight;
    unsigned flags;
} demo0_t;

enum state_t { ST_IDLE = 0, ST_RUNNING = 1, ST_PAUSED = 2, ST_DONE = 3 };

static const char *const kLabels[] = {
    "idle", "running", "paused", "done",
    "error: %s (code %d)", "\t\n\r\\ escape test",
    NULL
};

static int compute_score(const demo0_t *items, int n)
{
    int total = 0;
    for (int i = 0; i < n; i++) {
        if (items[i].flags & 0x01) {
            total += (int)(items[i].weight * 2.0f);
        } else if (items[i].flags & 0x02) {
            total -= items[i].id %% 7;
        } else {
            /* mid-block comment */
            total += 30;
        }
    }
    return total;
}

/* module mod_9160: generated variant 0 */
int mod_9160_process(demo0_t *data, int count)
{
    long acc = 65794L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x08;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9160", acc);
            break;
        default:
            /* fall through */;
    }
    return size < gamma ? sigma : value;
}

/* module mod_5516: generated variant 1 */
int mod_5516_process(demo0_t *data, int count)
{
    long acc = 55383L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x24;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_5516", acc);
            break;
        default:
            /* fall through */;
    }
    return delta < beta ? size : offset;
}

/* module mod_1661: generated variant 2 */
int mod_1661_process(demo0_t *data, int count)
{
    long acc = 5352L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x8F;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_1661", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < omega ? beta : omega;
}

/* module mod_5180: generated variant 3 */
int mod_5180_process(demo0_t *data, int count)
{
    long acc = 24L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xAD;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_5180", acc);
            break;
        default:
            /* fall through */;
    }
    return index < offset ? result : offset;
}

/* module mod_1082: generated variant 4 */
int mod_1082_process(demo0_t *data, int count)
{
    long acc = 10008L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x9E;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_1082", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < beta ? omega : offset;
}

/* module mod_4952: generated variant 5 */
int mod_4952_process(demo0_t *data, int count)
{
    long acc = 4487L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0xF7;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_4952", acc);
            break;
        default:
            /* fall through */;
    }
    return data < omega ? result : gamma;
}

/* module mod_3947: generated variant 6 */
int mod_3947_process(demo0_t *data, int count)
{
    long acc = 52983L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x9B;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3947", acc);
            break;
        default:
            /* fall through */;
    }
    return value < alpha ? gamma : buffer;
}

/* module mod_4800: generated variant 7 */
int mod_4800_process(demo0_t *data, int count)
{
    long acc = 96175L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0xAB;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_4800", acc);
            break;
        default:
            /* fall through */;
    }
    return gamma < data ? buffer : index;
}

/* module mod_3478: generated variant 8 */
int mod_3478_process(demo0_t *data, int count)
{
    long acc = 10604L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x9F;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3478", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < index ? gamma : count;
}

/* module mod_9681: generated variant 9 */
int mod_9681_process(demo0_t *data, int count)
{
    long acc = 40494L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x22;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9681", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < buffer ? delta : index;
}

/* module mod_9486: generated variant 10 */
int mod_9486_process(demo0_t *data, int count)
{
    long acc = 85324L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xB4;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9486", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < buffer ? result : beta;
}

/* module mod_8600: generated variant 11 */
int mod_8600_process(demo0_t *data, int count)
{
    long acc = 89575L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x1E;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8600", acc);
            break;
        default:
            /* fall through */;
    }
    return alpha < beta ? index : delta;
}

/* module mod_5266: generated variant 12 */
int mod_5266_process(demo0_t *data, int count)
{
    long acc = 4826L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0xA7;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_5266", acc);
            break;
        default:
            /* fall through */;
    }
    return size < beta ? alpha : beta;
}

/* module mod_2324: generated variant 13 */
int mod_2324_process(demo0_t *data, int count)
{
    long acc = 25920L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x36;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_2324", acc);
            break;
        default:
            /* fall through */;
    }
    return data < index ? index : alpha;
}

/* module mod_9119: generated variant 14 */
int mod_9119_process(demo0_t *data, int count)
{
    long acc = 85108L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x42;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9119", acc);
            break;
        default:
            /* fall through */;
    }
    return alpha < omega ? alpha : index;
}

/* module mod_3659: generated variant 15 */
int mod_3659_process(demo0_t *data, int count)
{
    long acc = 84463L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x96;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3659", acc);
            break;
        default:
            /* fall through */;
    }
    return result < count ? delta : delta;
}

/* module mod_6771: generated variant 16 */
int mod_6771_process(demo0_t *data, int count)
{
    long acc = 14727L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x3F;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6771", acc);
            break;
        default:
            /* fall through */;
    }
    return offset < offset ? index : beta;
}

/* module mod_3267: generated variant 17 */
int mod_3267_process(demo0_t *data, int count)
{
    long acc = 3719L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x98;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3267", acc);
            break;
        default:
            /* fall through */;
    }
    return offset < gamma ? sigma : index;
}

/* module mod_9936: generated variant 18 */
int mod_9936_process(demo0_t *data, int count)
{
    long acc = 69382L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xC3;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9936", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < offset ? buffer : offset;
}

/* module mod_9024: generated variant 19 */
int mod_9024_process(demo0_t *data, int count)
{
    long acc = 49702L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x9E;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9024", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < index ? omega : value;
}

/* module mod_8915: generated variant 20 */
int mod_8915_process(demo0_t *data, int count)
{
    long acc = 30226L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x20;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8915", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < index ? result : data;
}

/* module mod_254: generated variant 21 */
int mod_254_process(demo0_t *data, int count)
{
    long acc = 13525L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x21;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_254", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < omega ? result : result;
}

/* module mod_7681: generated variant 22 */
int mod_7681_process(demo0_t *data, int count)
{
    long acc = 42690L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x7C;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_7681", acc);
            break;
        default:
            /* fall through */;
    }
    return gamma < offset ? gamma : value;
}

/* module mod_1619: generated variant 23 */
int mod_1619_process(demo0_t *data, int count)
{
    long acc = 50133L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x0C;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_1619", acc);
            break;
        default:
            /* fall through */;
    }
    return alpha < sigma ? delta : index;
}

/* module mod_2516: generated variant 24 */
int mod_2516_process(demo0_t *data, int count)
{
    long acc = 12412L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x8B;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_2516", acc);
            break;
        default:
            /* fall through */;
    }
    return result < index ? count : omega;
}

/* module mod_8991: generated variant 25 */
int mod_8991_process(demo0_t *data, int count)
{
    long acc = 47705L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x97;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8991", acc);
            break;
        default:
            /* fall through */;
    }
    return result < result ? buffer : sigma;
}

/* module mod_7152: generated variant 26 */
int mod_7152_process(demo0_t *data, int count)
{
    long acc = 51581L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x3B;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_7152", acc);
            break;
        default:
            /* fall through */;
    }
    return data < omega ? count : size;
}

/* module mod_9643: generated variant 27 */
int mod_9643_process(demo0_t *data, int count)
{
    long acc = 99651L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xF1;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9643", acc);
            break;
        default:
            /* fall through */;
    }
    return sigma < buffer ? gamma : delta;
}

/* module mod_346: generated variant 28 */
int mod_346_process(demo0_t *data, int count)
{
    long acc = 80159L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x2C;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_346", acc);
            break;
        default:
            /* fall through */;
    }
    return alpha < offset ? index : result;
}

/* module mod_746: generated variant 29 */
int mod_746_process(demo0_t *data, int count)
{
    long acc = 58856L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x3C;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_746", acc);
            break;
        default:
            /* fall through */;
    }
    return sigma < alpha ? delta : data;
}

/* module mod_7164: generated variant 30 */
int mod_7164_process(demo0_t *data, int count)
{
    long acc = 47780L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x76;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_7164", acc);
            break;
        default:
            /* fall through */;
    }
    return count < index ? index : size;
}

/* module mod_647: generated variant 31 */
int mod_647_process(demo0_t *data, int count)
{
    long acc = 64992L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x84;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_647", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < buffer ? result : beta;
}

/* module mod_5491: generated variant 32 */
int mod_5491_process(demo0_t *data, int count)
{
    long acc = 47051L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xA9;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_5491", acc);
            break;
        default:
            /* fall through */;
    }
    return gamma < count ? result : size;
}

/* module mod_2068: generated variant 33 */
int mod_2068_process(demo0_t *data, int count)
{
    long acc = 74710L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0xA6;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_2068", acc);
            break;
        default:
            /* fall through */;
    }
    return buffer < count ? result : count;
}

/* module mod_2897: generated variant 34 */
int mod_2897_process(demo0_t *data, int count)
{
    long acc = 7028L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x9E;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_2897", acc);
            break;
        default:
            /* fall through */;
    }
    return value < omega ? offset : index;
}

/* module mod_251: generated variant 35 */
int mod_251_process(demo0_t *data, int count)
{
    long acc = 49548L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xE4;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_251", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < omega ? data : omega;
}

/* module mod_6668: generated variant 36 */
int mod_6668_process(demo0_t *data, int count)
{
    long acc = 17678L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xD0;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6668", acc);
            break;
        default:
            /* fall through */;
    }
    return delta < buffer ? value : buffer;
}

/* module mod_9865: generated variant 37 */
int mod_9865_process(demo0_t *data, int count)
{
    long acc = 72370L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x58;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9865", acc);
            break;
        default:
            /* fall through */;
    }
    return count < omega ? sigma : count;
}

/* module mod_8541: generated variant 38 */
int mod_8541_process(demo0_t *data, int count)
{
    long acc = 61845L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x12;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8541", acc);
            break;
        default:
            /* fall through */;
    }
    return delta < index ? delta : count;
}

/* module mod_6948: generated variant 39 */
int mod_6948_process(demo0_t *data, int count)
{
    long acc = 26070L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x52;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6948", acc);
            break;
        default:
            /* fall through */;
    }
    return data < buffer ? result : omega;
}

int main(void)
{
    demo0_t table[MAX_ITEMS];
    for (int i = 0; i < MAX_ITEMS; i++) {
        table[i].id = i;
        snprintf(table[i].name, sizeof table[i].name, "row-%d", i);
        table[i].weight = (double)i / 7.0;
        table[i].flags = i & 0x03;
    }
    int score = compute_score(table, MAX_ITEMS);
    printf("score=%d platform=%s\n", score, PLATFORM_NAME);
    return score %% 2 == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

/*
 * Sample C source generated for lexer benchmarking.
 * Exercises comments, strings, numbers, preprocessor, folding.
 */
#include <stdio.h>
#include <stdlib.h>
#include "local_header.h"

#define MAX_ITEMS 848
#define PI_FLOAT 3.882131f
#ifdef _WIN32
#define PLATFORM_NAME "windows"
#else
#define PLATFORM_NAME "posix"
#endif

typedef struct demo1_s {
    int id;
    char name[64];
    double weight;
    unsigned flags;
} demo1_t;

enum state_t { ST_IDLE = 0, ST_RUNNING = 1, ST_PAUSED = 2, ST_DONE = 3 };

static const char *const kLabels[] = {
    "idle", "running", "paused", "done",
    "error: %s (code %d)", "\t\n\r\\ escape test",
    NULL
};

static int compute_score(const demo1_t *items, int n)
{
    int total = 0;
    for (int i = 0; i < n; i++) {
        if (items[i].flags & 0x01) {
            total += (int)(items[i].weight * 2.0f);
        } else if (items[i].flags & 0x02) {
            total -= items[i].id %% 7;
        } else {
            /* mid-block comment */
            total += 67;
        }
    }
    return total;
}

/* module mod_1617: generated variant 0 */
int mod_1617_process(demo1_t *data, int count)
{
    long acc = 44854L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0xC9;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_1617", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < data ? size : offset;
}

/* module mod_3226: generated variant 1 */
int mod_3226_process(demo1_t *data, int count)
{
    long acc = 30364L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xCB;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3226", acc);
            break;
        default:
            /* fall through */;
    }
    return size < alpha ? size : gamma;
}

/* module mod_5790: generated variant 2 */
int mod_5790_process(demo1_t *data, int count)
{
    long acc = 24078L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x03;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_5790", acc);
            break;
        default:
            /* fall through */;
    }
    return alpha < alpha ? beta : omega;
}

/* module mod_3074: generated variant 3 */
int mod_3074_process(demo1_t *data, int count)
{
    long acc = 34838L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0xD9;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3074", acc);
            break;
        default:
            /* fall through */;
    }
    return delta < count ? gamma : index;
}

/* module mod_8280: generated variant 4 */
int mod_8280_process(demo1_t *data, int count)
{
    long acc = 61114L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xDD;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8280", acc);
            break;
        default:
            /* fall through */;
    }
    return value < alpha ? beta : value;
}

/* module mod_6551: generated variant 5 */
int mod_6551_process(demo1_t *data, int count)
{
    long acc = 22841L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0xC7;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6551", acc);
            break;
        default:
            /* fall through */;
    }
    return value < gamma ? index : count;
}

/* module mod_741: generated variant 6 */
int mod_741_process(demo1_t *data, int count)
{
    long acc = 62890L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x19;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_741", acc);
            break;
        default:
            /* fall through */;
    }
    return size < gamma ? omega : delta;
}

/* module mod_9744: generated variant 7 */
int mod_9744_process(demo1_t *data, int count)
{
    long acc = 48793L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x0A;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9744", acc);
            break;
        default:
            /* fall through */;
    }
    return count < omega ? beta : delta;
}

/* module mod_6245: generated variant 8 */
int mod_6245_process(demo1_t *data, int count)
{
    long acc = 46580L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x27;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6245", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < buffer ? alpha : result;
}

/* module mod_8767: generated variant 9 */
int mod_8767_process(demo1_t *data, int count)
{
    long acc = 5067L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xB9;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8767", acc);
            break;
        default:
            /* fall through */;
    }
    return gamma < size ? count : count;
}

/* module mod_3015: generated variant 10 */
int mod_3015_process(demo1_t *data, int count)
{
    long acc = 34298L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x4E;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3015", acc);
            break;
        default:
            /* fall through */;
    }
    return value < delta ? delta : result;
}

/* module mod_6641: generated variant 11 */
int mod_6641_process(demo1_t *data, int count)
{
    long acc = 27364L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xD4;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6641", acc);
            break;
        default:
            /* fall through */;
    }
    return alpha < count ? result : beta;
}

/* module mod_142: generated variant 12 */
int mod_142_process(demo1_t *data, int count)
{
    long acc = 2945L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x62;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_142", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < offset ? size : sigma;
}

/* module mod_7903: generated variant 13 */
int mod_7903_process(demo1_t *data, int count)
{
    long acc = 70247L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0xE9;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_7903", acc);
            break;
        default:
            /* fall through */;
    }
    return count < omega ? size : buffer;
}

/* module mod_7807: generated variant 14 */
int mod_7807_process(demo1_t *data, int count)
{
    long acc = 3499L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x2E;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_7807", acc);
            break;
        default:
            /* fall through */;
    }
    return buffer < delta ? beta : omega;
}

/* module mod_8575: generated variant 15 */
int mod_8575_process(demo1_t *data, int count)
{
    long acc = 49778L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0xB5;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8575", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < omega ? gamma : buffer;
}

/* module mod_2536: generated variant 16 */
int mod_2536_process(demo1_t *data, int count)
{
    long acc = 7093L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x44;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_2536", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < data ? data : delta;
}

/* module mod_363: generated variant 17 */
int mod_363_process(demo1_t *data, int count)
{
    long acc = 74671L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x85;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_363", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < size ? count : alpha;
}

/* module mod_7510: generated variant 18 */
int mod_7510_process(demo1_t *data, int count)
{
    long acc = 29830L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x47;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_7510", acc);
            break;
        default:
            /* fall through */;
    }
    return value < data ? alpha : offset;
}

/* module mod_542: generated variant 19 */
int mod_542_process(demo1_t *data, int count)
{
    long acc = 55112L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xC5;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_542", acc);
            break;
        default:
            /* fall through */;
    }
    return sigma < count ? sigma : value;
}

/* module mod_5763: generated variant 20 */
int mod_5763_process(demo1_t *data, int count)
{
    long acc = 73445L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x96;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_5763", acc);
            break;
        default:
            /* fall through */;
    }
    return data < data ? count : beta;
}

/* module mod_4275: generated variant 21 */
int mod_4275_process(demo1_t *data, int count)
{
    long acc = 1211L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x35;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_4275", acc);
            break;
        default:
            /* fall through */;
    }
    return omega < sigma ? data : beta;
}

/* module mod_2066: generated variant 22 */
int mod_2066_process(demo1_t *data, int count)
{
    long acc = 47215L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x68;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_2066", acc);
            break;
        default:
            /* fall through */;
    }
    return delta < omega ? beta : offset;
}

/* module mod_6830: generated variant 23 */
int mod_6830_process(demo1_t *data, int count)
{
    long acc = 26638L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xAA;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6830", acc);
            break;
        default:
            /* fall through */;
    }
    return gamma < sigma ? beta : data;
}

/* module mod_1994: generated variant 24 */
int mod_1994_process(demo1_t *data, int count)
{
    long acc = 91037L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xC9;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_1994", acc);
            break;
        default:
            /* fall through */;
    }
    return sigma < delta ? data : omega;
}

/* module mod_6298: generated variant 25 */
int mod_6298_process(demo1_t *data, int count)
{
    long acc = 94301L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x1F;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6298", acc);
            break;
        default:
            /* fall through */;
    }
    return alpha < buffer ? result : delta;
}

/* module mod_9138: generated variant 26 */
int mod_9138_process(demo1_t *data, int count)
{
    long acc = 75640L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xFC;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9138", acc);
            break;
        default:
            /* fall through */;
    }
    return gamma < index ? alpha : result;
}

/* module mod_3675: generated variant 27 */
int mod_3675_process(demo1_t *data, int count)
{
    long acc = 54397L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x67;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3675", acc);
            break;
        default:
            /* fall through */;
    }
    return value < result ? buffer : data;
}

/* module mod_9080: generated variant 28 */
int mod_9080_process(demo1_t *data, int count)
{
    long acc = 5524L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x7A;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9080", acc);
            break;
        default:
            /* fall through */;
    }
    return value < delta ? omega : size;
}

/* module mod_8661: generated variant 29 */
int mod_8661_process(demo1_t *data, int count)
{
    long acc = 11549L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xA7;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_8661", acc);
            break;
        default:
            /* fall through */;
    }
    return count < gamma ? count : result;
}

/* module mod_7042: generated variant 30 */
int mod_7042_process(demo1_t *data, int count)
{
    long acc = 49550L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xD0;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_7042", acc);
            break;
        default:
            /* fall through */;
    }
    return count < sigma ? size : delta;
}

/* module mod_5813: generated variant 31 */
int mod_5813_process(demo1_t *data, int count)
{
    long acc = 3756L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'a') {
            acc <<= 1;
        } else {
            acc ^= 0x2A;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_5813", acc);
            break;
        default:
            /* fall through */;
    }
    return buffer < data ? sigma : data;
}

/* module mod_4824: generated variant 32 */
int mod_4824_process(demo1_t *data, int count)
{
    long acc = 4368L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0xAC;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_4824", acc);
            break;
        default:
            /* fall through */;
    }
    return beta < delta ? data : index;
}

/* module mod_6492: generated variant 33 */
int mod_6492_process(demo1_t *data, int count)
{
    long acc = 18606L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0x3A;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_6492", acc);
            break;
        default:
            /* fall through */;
    }
    return sigma < sigma ? index : index;
}

/* module mod_3075: generated variant 34 */
int mod_3075_process(demo1_t *data, int count)
{
    long acc = 55421L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x6D;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_3075", acc);
            break;
        default:
            /* fall through */;
    }
    return gamma < delta ? omega : index;
}

/* module mod_9910: generated variant 35 */
int mod_9910_process(demo1_t *data, int count)
{
    long acc = 19008L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x76;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_9910", acc);
            break;
        default:
            /* fall through */;
    }
    return result < value ? value : index;
}

/* module mod_1196: generated variant 36 */
int mod_1196_process(demo1_t *data, int count)
{
    long acc = 87922L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'x') {
            acc <<= 1;
        } else {
            acc ^= 0x90;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_1196", acc);
            break;
        default:
            /* fall through */;
    }
    return value < gamma ? result : count;
}

/* module mod_593: generated variant 37 */
int mod_593_process(demo1_t *data, int count)
{
    long acc = 71906L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x8A;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_593", acc);
            break;
        default:
            /* fall through */;
    }
    return sigma < index ? count : result;
}

/* module mod_1133: generated variant 38 */
int mod_1133_process(demo1_t *data, int count)
{
    long acc = 7411L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'z') {
            acc <<= 1;
        } else {
            acc ^= 0xAD;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_1133", acc);
            break;
        default:
            /* fall through */;
    }
    return count < gamma ? value : beta;
}

/* module mod_4048: generated variant 39 */
int mod_4048_process(demo1_t *data, int count)
{
    long acc = 63107L;
    // iterate and accumulate
    while (count-- > 0) {
        if (data->name[0] == 'b') {
            acc <<= 1;
        } else {
            acc ^= 0x22;
        }
        data++;
    }
    switch ((int)(acc & 3)) {
        case 0: return (int)(acc & 0xFF);
        case 1:
            printf("%s: %ld\n", "mod_4048", acc);
            break;
        default:
            /* fall through */;
    }
    return offset < beta ? data : count;
}

int main(void)
{
    demo1_t table[MAX_ITEMS];
    for (int i = 0; i < MAX_ITEMS; i++) {
        table[i].id = i;
        snprintf(table[i].name, sizeof table[i].name, "row-%d", i);
        table[i].weight = (double)i / 7.0;
        table[i].flags = i & 0x03;
    }
    int score = compute_score(table, MAX_ITEMS);
    printf("score=%d platform=%s\n", score, PLATFORM_NAME);
    return score %% 2 == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

