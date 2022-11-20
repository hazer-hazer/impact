interface ADTValue {
    __$tag: string
    __$values: any[]
}

export function isADTValue(value: any): value is ADTValue {
    return typeof value === 'object' && '__$tag' in value && '__$values' in value
}

export function valueStr(value: any): string {
    if (isADTValue(value)) {
        return `${value.__$tag} ${value.__$values.map(valueStr).join(' ')}`
    }

    return value
}
