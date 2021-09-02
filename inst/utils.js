// Given a json object, return an object of json strings corresponding
// to each entry. This does require one round of parse/stringify and
// more direct access to a json parser might be more efficient (if we
// knew the start/end positions of each entry we could clip them from
// the input string).
function jsonParseDepth1(x) {
    if (x.length === 0 || x[0] !== "{") {
        throw "Provided json is not an object";
    }
    var data = JSON.parse(x);
    for (const key of Object.keys(data)) {
        data[key] = JSON.stringify(data[key]);
    }
    return data;
}

function jsonParseExtract(x, key) {
    if (x.length === 0 || x[0] !== "{") {
        throw "Provided json is not an object";
    }
    var data = JSON.parse(x);
    if (!(key in data)) {
        throw "Did not find key '" + key + "' within object";
    }
    return JSON.stringify(data[key]);
}
