export interface RuleSet {
    setId: number,
    rules: number[]
}

export interface Slice {
    slice_id: number,
    loc: [[number, number], [number, number]],
    appears: number[]
}

export interface TypeError {
    error_id: number,
    mus_list: RuleSet[],
    mcs_list: RuleSet[],
    slices: Slice[]
}

export interface Hole {
    original: string,
    hole_id: number,
    loc: [[number, number], [number, number]],
    signature: string,
    kind: 'Type hole' | 'Type wildcard' | 'Rename',

    error_id: number
}
