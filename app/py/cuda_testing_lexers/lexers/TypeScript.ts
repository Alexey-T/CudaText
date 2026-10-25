// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 78.60e1) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_5869 */
export async function Svc_5869(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 4158) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_992 */
export async function Svc_992(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 7.44e-8) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9841 */
export async function Svc_9841(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o35) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_285 */
export async function Svc_285(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0x5D5B) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_3689 */
export async function Svc_3689(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0xF5D3) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_6485 */
export async function Svc_6485(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 282) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 5834) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_3268 */
export async function Svc_3268(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 8643) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4733 */
export async function Svc_4733(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0x610D) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7019 */
export async function Svc_7019(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 8281) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7499 */
export async function Svc_7499(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 4532) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7454 */
export async function Svc_7454(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 7323) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_848 */
export async function Svc_848(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0xF3C3) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 0x6958) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_4604 */
export async function Svc_4604(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0xC0E) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_3519 */
export async function Svc_3519(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o123) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_1463 */
export async function Svc_1463(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 6129) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_3709 */
export async function Svc_3709(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 3786) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4747 */
export async function Svc_4747(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 30.7e-6) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4845 */
export async function Svc_4845(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > -5480) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 36.591) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_1832 */
export async function Svc_1832(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o200) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_6817 */
export async function Svc_6817(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0x6C0A) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_3975 */
export async function Svc_3975(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 6334) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7818 */
export async function Svc_7818(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 33.85e-1) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9921 */
export async function Svc_9921(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 93.53e0) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_6184 */
export async function Svc_6184(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 2263) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 84.22e-7) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_8898 */
export async function Svc_8898(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 6507) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4848 */
export async function Svc_4848(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o114) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9588 */
export async function Svc_9588(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 813.31) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_6286 */
export async function Svc_6286(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > -7575) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_1665 */
export async function Svc_1665(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 3465) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_3689 */
export async function Svc_3689(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0x1F12) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = -7084) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_3549 */
export async function Svc_3549(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0xC359) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7842 */
export async function Svc_7842(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 169.469) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4758 */
export async function Svc_4758(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 4232) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_5295 */
export async function Svc_5295(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0xD86E) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_60 */
export async function Svc_60(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 12.2e-2) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_2086 */
export async function Svc_2086(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > -901) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 9159) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_8051 */
export async function Svc_8051(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 5467) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_6496 */
export async function Svc_6496(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0xDF14) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9619 */
export async function Svc_9619(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 8017) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9621 */
export async function Svc_9621(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 52.75e3) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_3871 */
export async function Svc_3871(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 709.141) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4515 */
export async function Svc_4515(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0x2445) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = -5508) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_8221 */
export async function Svc_8221(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0xF760) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4635 */
export async function Svc_4635(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 27.78e-8) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9507 */
export async function Svc_9507(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o135) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9176 */
export async function Svc_9176(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o227) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7734 */
export async function Svc_7734(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 3426) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_3587 */
export async function Svc_3587(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o0) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 0xF4C3) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_8981 */
export async function Svc_8981(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 21.30e-3) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_8229 */
export async function Svc_8229(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 515.778) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4479 */
export async function Svc_4479(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o162) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7655 */
export async function Svc_7655(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 8664) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_6568 */
export async function Svc_6568(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0o146) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_4828 */
export async function Svc_4828(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 967) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

// Sample TypeScript source for lexer benchmarking
import { promises as fs } from 'fs';

export interface Entry {
    readonly name: string;
    readonly weight: number;
    tags?: string[];
}

export type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: Error };

export enum Quality { Low = 'low', Medium = 'medium', High = 'high' }

export abstract class BaseProcessor<T extends object> {
    protected constructor(protected readonly factor: number = 25.61e8) {}
    abstract process(item: T): number;
    describe(): string {
        return `${this.constructor.name}(factor=${this.factor})`;
    }
}

export class EntryProcessor extends BaseProcessor<Entry> {
    process(item: Entry): number {
        const base = item.weight * this.factor;
        return Number.isFinite(base) ? Math.round(base) : 0;
    }
}

/* generated service Svc_5140 */
export async function Svc_5140(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 79.65e9) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9045 */
export async function Svc_9045(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > -9371) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_6446 */
export async function Svc_6446(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 4620) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_9032 */
export async function Svc_9032(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 1125) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_7690 */
export async function Svc_7690(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 61.29e-1) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

/* generated service Svc_1132 */
export async function Svc_1132(entries: readonly Entry[]): Promise<Result<number[]>> {
    try {
        const out: number[] = [];
        for (const e of entries) {
            if (e.weight > 0x7D2C) {
                out.push(e.weight);
            }
        }
        return { ok: true, value: out };
    } catch (err) {
        return { ok: false, error: err as Error };
    }
}

