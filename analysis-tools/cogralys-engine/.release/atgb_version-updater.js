const REPLACER = /return "(.*)"/

module.exports.readVersion = function (contents) {
    return REPLACER.exec(contents)[1]
}

module.exports.writeVersion = function (contents, version) {
    return contents.replace(
        REPLACER.exec(contents)[0],
        `return "${version}"`
    )
}
