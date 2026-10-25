// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 1339;
const PREFIX = "bench-";
const FACTOR = 0.719257472;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_7893 */
const handler_7893 = async (seed, opts = {}) => {
    const { scale = 74, label = 'handler_7893' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xDC;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6053 */
const handler_6053 = async (seed, opts = {}) => {
    const { scale = 23, label = 'handler_6053' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xBB;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1800 */
const handler_1800 = async (seed, opts = {}) => {
    const { scale = 35, label = 'handler_1800' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xAE;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6729 */
const handler_6729 = async (seed, opts = {}) => {
    const { scale = 96, label = 'handler_6729' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x12;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_2403 */
const handler_2403 = async (seed, opts = {}) => {
    const { scale = 44, label = 'handler_2403' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x55;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_8843 */
const handler_8843 = async (seed, opts = {}) => {
    const { scale = 39, label = 'handler_8843' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x8A;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_3908-0', 5936),
    makeEntry('e_457-1', -779),
    makeEntry('e_1987-2', 0xCACD),
    makeEntry('e_4104-3', 3200),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 1089;
const PREFIX = "bench-";
const FACTOR = 0.604809562;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_2146 */
const handler_2146 = async (seed, opts = {}) => {
    const { scale = 70, label = 'handler_2146' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x05;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6129 */
const handler_6129 = async (seed, opts = {}) => {
    const { scale = 96, label = 'handler_6129' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x75;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_355 */
const handler_355 = async (seed, opts = {}) => {
    const { scale = 83, label = 'handler_355' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x45;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_8010 */
const handler_8010 = async (seed, opts = {}) => {
    const { scale = 77, label = 'handler_8010' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x0A;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_8053 */
const handler_8053 = async (seed, opts = {}) => {
    const { scale = 5, label = 'handler_8053' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x95;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1362 */
const handler_1362 = async (seed, opts = {}) => {
    const { scale = 84, label = 'handler_1362' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x2F;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_8682-0', 83.83e9),
    makeEntry('e_9991-1', 223.1),
    makeEntry('e_6399-2', 0xD12C),
    makeEntry('e_4711-3', 0o450),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 6109;
const PREFIX = "bench-";
const FACTOR = 0.234430693;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_7836 */
const handler_7836 = async (seed, opts = {}) => {
    const { scale = 49, label = 'handler_7836' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xD9;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_9379 */
const handler_9379 = async (seed, opts = {}) => {
    const { scale = 16, label = 'handler_9379' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xC0;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_8807 */
const handler_8807 = async (seed, opts = {}) => {
    const { scale = 71, label = 'handler_8807' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x0C;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_8462 */
const handler_8462 = async (seed, opts = {}) => {
    const { scale = 64, label = 'handler_8462' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xE0;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6267 */
const handler_6267 = async (seed, opts = {}) => {
    const { scale = 12, label = 'handler_6267' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x29;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_7508 */
const handler_7508 = async (seed, opts = {}) => {
    const { scale = 96, label = 'handler_7508' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x64;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_2331-0', 0xB467),
    makeEntry('e_7181-1', 0xFF87),
    makeEntry('e_830-2', 0o540),
    makeEntry('e_6770-3', 0o21),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 4546;
const PREFIX = "bench-";
const FACTOR = 0.487208075;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_2691 */
const handler_2691 = async (seed, opts = {}) => {
    const { scale = 69, label = 'handler_2691' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xDD;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_7676 */
const handler_7676 = async (seed, opts = {}) => {
    const { scale = 79, label = 'handler_7676' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x42;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_8459 */
const handler_8459 = async (seed, opts = {}) => {
    const { scale = 98, label = 'handler_8459' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xDC;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1023 */
const handler_1023 = async (seed, opts = {}) => {
    const { scale = 57, label = 'handler_1023' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xCD;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6759 */
const handler_6759 = async (seed, opts = {}) => {
    const { scale = 66, label = 'handler_6759' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xCA;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6552 */
const handler_6552 = async (seed, opts = {}) => {
    const { scale = 45, label = 'handler_6552' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x66;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_3025-0', 77.62e-6),
    makeEntry('e_8864-1', 0o163),
    makeEntry('e_371-2', 0x1C84),
    makeEntry('e_8155-3', 7737),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 2795;
const PREFIX = "bench-";
const FACTOR = 0.336458566;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_2470 */
const handler_2470 = async (seed, opts = {}) => {
    const { scale = 95, label = 'handler_2470' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xC9;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1148 */
const handler_1148 = async (seed, opts = {}) => {
    const { scale = 59, label = 'handler_1148' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x8F;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_9568 */
const handler_9568 = async (seed, opts = {}) => {
    const { scale = 87, label = 'handler_9568' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x77;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_3533 */
const handler_3533 = async (seed, opts = {}) => {
    const { scale = 2, label = 'handler_3533' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xD2;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_182 */
const handler_182 = async (seed, opts = {}) => {
    const { scale = 15, label = 'handler_182' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xB3;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1160 */
const handler_1160 = async (seed, opts = {}) => {
    const { scale = 76, label = 'handler_1160' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x11;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_3822-0', 0o304),
    makeEntry('e_1284-1', -5187),
    makeEntry('e_7745-2', 564.112),
    makeEntry('e_6837-3', 325.971),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 4009;
const PREFIX = "bench-";
const FACTOR = 0.861797868;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_9063 */
const handler_9063 = async (seed, opts = {}) => {
    const { scale = 46, label = 'handler_9063' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x62;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_3426 */
const handler_3426 = async (seed, opts = {}) => {
    const { scale = 46, label = 'handler_3426' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xAD;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1300 */
const handler_1300 = async (seed, opts = {}) => {
    const { scale = 23, label = 'handler_1300' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x47;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_5490 */
const handler_5490 = async (seed, opts = {}) => {
    const { scale = 71, label = 'handler_5490' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x75;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_148 */
const handler_148 = async (seed, opts = {}) => {
    const { scale = 50, label = 'handler_148' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xFB;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6151 */
const handler_6151 = async (seed, opts = {}) => {
    const { scale = 89, label = 'handler_6151' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x89;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_7497-0', 44.33e7),
    makeEntry('e_42-1', 0x67F4),
    makeEntry('e_5503-2', 6632),
    makeEntry('e_9869-3', 838.336),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 2593;
const PREFIX = "bench-";
const FACTOR = 0.339100572;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_8227 */
const handler_8227 = async (seed, opts = {}) => {
    const { scale = 78, label = 'handler_8227' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x41;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_9329 */
const handler_9329 = async (seed, opts = {}) => {
    const { scale = 45, label = 'handler_9329' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x8E;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_7950 */
const handler_7950 = async (seed, opts = {}) => {
    const { scale = 82, label = 'handler_7950' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xDD;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_7139 */
const handler_7139 = async (seed, opts = {}) => {
    const { scale = 95, label = 'handler_7139' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x16;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1932 */
const handler_1932 = async (seed, opts = {}) => {
    const { scale = 78, label = 'handler_1932' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x3E;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6512 */
const handler_6512 = async (seed, opts = {}) => {
    const { scale = 42, label = 'handler_6512' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x34;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_2765-0', 876.990),
    makeEntry('e_4623-1', 546.956),
    makeEntry('e_8227-2', 0o333),
    makeEntry('e_2385-3', 5771),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 4280;
const PREFIX = "bench-";
const FACTOR = 0.957019717;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_2929 */
const handler_2929 = async (seed, opts = {}) => {
    const { scale = 87, label = 'handler_2929' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xD1;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_3593 */
const handler_3593 = async (seed, opts = {}) => {
    const { scale = 48, label = 'handler_3593' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xA3;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_1704 */
const handler_1704 = async (seed, opts = {}) => {
    const { scale = 23, label = 'handler_1704' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x4F;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_3336 */
const handler_3336 = async (seed, opts = {}) => {
    const { scale = 70, label = 'handler_3336' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x16;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_9033 */
const handler_9033 = async (seed, opts = {}) => {
    const { scale = 97, label = 'handler_9033' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x55;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_7480 */
const handler_7480 = async (seed, opts = {}) => {
    const { scale = 99, label = 'handler_7480' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x06;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_56-0', 4003),
    makeEntry('e_4275-1', 2892),
    makeEntry('e_892-2', 669),
    makeEntry('e_5924-3', 2673),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

// Sample JavaScript source for lexer benchmarking
'use strict';

const MAX_ITEMS = 5750;
const PREFIX = "bench-";
const FACTOR = 0.275327326;
let globalCounter = 0;

/**
 * Create one entry object.
 * @param {string} name
 * @param {number} weight
 * @returns {{name: string, weight: number, id: number}}
 */
function makeEntry(name, weight) {
    return {
        name,
        weight,
        id: ++globalCounter,
        tags: ['alpha', 'beta'],
        meta: { created: new Date().toISOString(), factor: FACTOR },
    };
}

class Accumulator {
    constructor(initial = 0) {
        this.total = initial;
        this.items = [];
    }

    add(value) {
        if (typeof value !== 'number') {
            throw new TypeError(`expected number, got ${typeof value}`);
        }
        this.total += value;
        this.items.push(value);
        return this;
    }

    get average() {
        return this.items.length
            ? this.total / this.items.length
            : 0;
    }
}

/* generated handler handler_1405 */
const handler_1405 = async (seed, opts = {}) => {
    const { scale = 43, label = 'handler_1405' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xD7;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_3378 */
const handler_3378 = async (seed, opts = {}) => {
    const { scale = 43, label = 'handler_3378' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xDE;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_9304 */
const handler_9304 = async (seed, opts = {}) => {
    const { scale = 63, label = 'handler_9304' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0xFE;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_3399 */
const handler_3399 = async (seed, opts = {}) => {
    const { scale = 35, label = 'handler_3399' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x48;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_6207 */
const handler_6207 = async (seed, opts = {}) => {
    const { scale = 68, label = 'handler_6207' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x52;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

/* generated handler handler_5783 */
const handler_5783 = async (seed, opts = {}) => {
    const { scale = 13, label = 'handler_5783' } = opts;
    let acc = 0;
    for (let i = 0; i < MAX_ITEMS; i++) {
        const v = (seed * i) ^ 0x7A;
        if (v % 2 === 0) {
            acc += v / scale;
        } else {
            acc -= v % scale;
        }
    }
    return `${label}:${acc.toFixed(3)}`;
};

const pipeline = [
    makeEntry('e_7336-0', 1),
    makeEntry('e_2371-1', 70.67e8),
    makeEntry('e_2450-2', 2029),
    makeEntry('e_1880-3', 0o350),
];

(async () => {
    const acc = new Accumulator();
    for (const entry of pipeline) {
        acc.add(entry.weight);
    }
    console.log(`average=${acc.average}, count=${acc.items.length}`);
})().catch((err) => {
    console.error('pipeline failed:', err);
    process.exitCode = 1;
});

