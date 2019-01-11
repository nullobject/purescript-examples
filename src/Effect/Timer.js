exports.setTimeout = function (ms) {
  return function (fn) {
    return function () {
      setTimeout(fn, ms);
    };
  };
};
