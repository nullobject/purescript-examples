exports.timer = function (ms) {
  return function (fn) {
    return function () {
      return setTimeout(function () { fn(Math.random())() }, ms);
    };
  };
};

exports.intervalImpl = function timerImpl (signal, delay) {
  return function (fn) {
    return signal(function () {
      setInterval(function () { fn(Math.random())(); }, delay);
    });
  };
};
