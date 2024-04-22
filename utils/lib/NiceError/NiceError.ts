/**
 * NiceError.ts v1.2.0
 */

/**
 * Options required for the constructor
 */
export interface NEOptions {
    name?: string,
    chain?: string[],
    info?: { [key: string]: unknown },
    cause?: Error | NiceError | null,
    stack?: string
}

/**
 * NiceError class
 */
export class NiceError {
    name?: string = 'NiceError'
    message = 'Empty'
    chain: string[] = []
    info: { [key: string]: unknown } = {}
    cause: Error | NiceError | null = null
    stack: string = (new Error).stack || ''

    static execPath = ''

    constructor(opts?: NEOptions) {
        if (opts && opts as NEOptions) {
            // Friendly reminder for bad parameters, but execution is not affected
            const keys = Object.keys(opts)
            const badParams: string[] = []
            for (const key of keys) {
                if (Object.keys(this).indexOf(key) < 0) {
                    badParams.push(key)
                }
            }
            if (badParams.length > 0) {
                console.log('Warning!!! You have provided bad parameter(s): [' + badParams.join(',') + '], it will be ignored, but we strongly suggest you to check your code again!')
            }
            if (opts.name) this.name = opts.name
            if (opts.chain) this.chain = opts.chain
            if (opts.cause) this.cause = opts.cause
            if (opts.info) this.info = opts.info
            // Error stack information
            if (opts.stack) this.stack = opts.stack
        }
        // Replace default error message with the full error message chain
        this.stack = this.stack.replace('Error', this.fullMessage())
        // Optimize error stack display path
        this.stack = this._removeSelfFromStack(this.stack)
        // Shorten file paths to relative directories
        this.stack = this._removeCWD(this.stack)
    }

    /**
     * Returns the complete error message of the instance
     * @returns Error message string
     */
    public fullMessage(): string {
        return this._getCauseMessage(this)
    }
    private _getCauseMessage(err: unknown): string {
        let result = ''
        // If it is an instance of NiceError or Error
        if (err instanceof Error) result = `[${err.name}]: ${err.message}`
        else if (err instanceof NiceError) {
            result = `[${err.name}${err.chain.length > 0 ? '@' + err.chain.join('/') : ''}]: ${err.message}`
            // If there is a sub-error, dive deeper
            if (err.cause) result += ' <= ' + this._getCauseMessage(err.cause)
        }
        // Otherwise, it is a third-party error or another object thrown
        else {
            result = '[Throw]: type = ' + typeof err
            const str = JSON.stringify(err)
            result += ', content = ' + str
        }
        return result
    }

    /**
     * Returns the complete stack information of the error
     * @returns Stack information string
     */
    public fullStack(): string {
        // Recursively obtain
        let fstack = this._getFullStack(this, true)
        // Remove NiceError file line
        fstack = this._removeSelfFromStack(fstack)
        // Shorten code paths
        fstack = this._removeCWD(fstack)
        return fstack
    }
    private _getFullStack(err: Error | NiceError, isFirst?: boolean): string {
        let result = ''
        let causedBy = ''
        if (isFirst !== true) causedBy = 'Caused by '
        // If it is an instance of NiceError, directly get the property
        if (err instanceof NiceError) {
            result = causedBy + err.stack
            if (err.cause) result += `\r\n` + this._getFullStack(err.cause)
        }
        // If it is an instance of Error, assemble it
        else if (err instanceof Error && err.stack) {
            result = causedBy + err.stack.replace(err.name, '[' + err.name + ']')
        }
        return result
    }

    /**
     * Retrieves the complete error detail information
     * @returns Complete error detail object
     */
    public fullInfo(): Record<string, unknown> {
        return this._getFullInfo(this)
    }
    private _getFullInfo(ne: NiceError): Record<string, unknown> {
        // Recursively obtain the information of sub-errors and merge them
        const result: Record<string, unknown> = {}
        if (ne instanceof NiceError) {
            const keys = Object.keys(ne.info)
            for (let i = 0; i < keys.length; i++) {
                const key = keys[i]
                result[key] = ne.info[key]
            }
            // If the same name info is set in different layers of an NE chain, the inner layer will overwrite the outer layer
            if (ne.cause && ne.cause instanceof NiceError) {
                const subInfo = this._getFullInfo(ne.cause)
                const keys = Object.keys(subInfo)
                for (let i = 0; i < keys.length; i++) {
                    const key = keys[i]
                    result[key] = subInfo[key]
                }
            }
        }
        return result
    }

    /**
     * Remove NiceError.js line from stack string
     * @param str Stack string
     * @returns Replaced content
     */
    private _removeSelfFromStack(str: string): string {
        const jsRegExp = /\s{1,}?at [ \S]*?NiceError[\S]*? \(\S*?\/NiceError.js:\d*:\d*\)[\n\r]{1,}/g
        const tsRegExp = /\s{1,}?at [ \S]*?NiceError[\S]*? \(\S*?\/NiceError.ts:\d*:\d*\)[\n\r]{1,}/g
        return str.replace(jsRegExp, `\r\n`).replace(tsRegExp, `\r\n`).replace(/(\r\n){2,}/g, `\r\n`).replace(/file:\/\//g, ``) // Note: replace multiple consecutive \r\n
    }

    /**
     * Remove the current running directory prefix (to make stack information easier to read)
     * @param str String to process
     * @returns Processed result
     */
    private _removeCWD(str: string): string {
        if (NiceError.execPath !== '') {
            // Convert the target string to the string required by RegExp, this conversion is very subtle, please pay attention to it :)
            const regStr = NiceError.execPath.replace(/\//g, `\\/`)
            const regExp = new RegExp(regStr, 'g')
            return str.replace(regExp, `.`)
        }
        return str
    }
}

export default NiceError
