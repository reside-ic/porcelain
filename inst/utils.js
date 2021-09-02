/*eslint no-unused-vars: "off"*/
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
