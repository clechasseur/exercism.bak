export class InputCell {
    constructor(value) {
        this._value = value;
        this._dependents = [];
    }
    get value() {
        return this._value;
    }
    setValue(value) {
        this._value = value;
        
        let dependentsToUpdate = [].concat(this._dependents);
        let callbacksToCall = new Map();
        while (dependentsToUpdate.length != 0) {
            let dependent = dependentsToUpdate.shift();
            if (dependent.update()) {
                dependentsToUpdate = dependentsToUpdate.concat(dependent._dependents);
                let existingCallbacks = callbacksToCall.get(dependent);
                if (existingCallbacks == undefined) {
                    existingCallbacks = new Set();
                    callbacksToCall.set(dependent, existingCallbacks);
                }
                dependent._callbacks.forEach((callback) => existingCallbacks.add(callback));
            }
        }
        callbacksToCall.forEach((callbacks, cell) =>
            callbacks.forEach((callback) =>
                callback.call(cell)
            )
        );
    }
}

export class ComputeCell {
    constructor(inputs, calc) {
        this._inputs = inputs;
        this._calc = calc;
        this._dependents = [];
        this._callbacks = [];
        this._inputs.forEach((input) => input._dependents.push(this));
        this.update();
    }
    get value() {
        return this._value;
    }
    addCallback(callback) {
        this._callbacks.push(callback);
    }
    removeCallback(callback) {
        let idx = this._callbacks.indexOf(callback);
        if (idx != -1) {
            this._callbacks.splice(idx, 1);
        }
    }
    update() {
        let value = this._calc(this._inputs);
        if (value != this._value) {
            this._value = value;
            return true;
        }
        return false;
    }
}

export class CallbackCell {
    constructor(fn) {
        this._fn = fn;
        this._values = [];
    }
    get values() {
        return this._values;
    }
    call(cell) {
        this._values.push(this._fn(cell));
    }
}
