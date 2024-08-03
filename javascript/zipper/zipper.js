const Direction = {
    LEFT: 'left',
    RIGHT: 'right'
};

export class Zipper {
    constructor(tree, path = []) {
        this._tree = JSON.parse(JSON.stringify(tree));
        this._path = path;
    }

    static fromTree(tree) {
        return new Zipper(tree);
    }
    toTree() {
        return this._tree;
    }

    node() {
        let node = this._tree;
        for (let dir of this._path) {
            if (!node) {
                break;
            }
            node = (dir === Direction.LEFT) ? node.left : node.right;
        }
        return node; 
    }

    value() {
        const node = this.node();
        return node ? node.value : null;
    }
    left() {
        const node = this.node();
        return node && node.left ? new Zipper(this._tree, this._path.concat([Direction.LEFT])) : null;
    }
    right() {
        const node = this.node();
        return node && node.right ? new Zipper(this._tree, this._path.concat([Direction.RIGHT])) : null;
    }
    up() {
        return this._path.length != 0 ? new Zipper(this._tree, this._path.slice(0, -1)) : null;
    }

    setValue(newValue) {
        const node = this.node();
        if (node) {
            node.value = newValue;
        }
        return this;
    }
    setLeft(newLeft) {
        const node = this.node();
        if (node) {
            node.left = newLeft;
        }
        return this;
    }
    setRight(newRight) {
        const node = this.node();
        if (node) {
            node.right = newRight;
        }
        return this;
    }
}
