export class BinarySearchTree {
  constructor(data) {
    this.data = data;
    this.left = null;
    this.right = null;
  }

  insert(data) {
    if (data <= this.data) {
      if (this.left === null) {
        this.left = new BinarySearchTree(data);
      } else {
        this.left.insert(data);
      }
    } else {
      if (this.right === null) {
        this.right = new BinarySearchTree(data);
      } else {
        this.right.insert(data);
      }
    }
  }

  each(callback) {
    if (this.left !== null) {
      this.left.each(callback);
    }
    callback(this.data);
    if (this.right !== null) {
      this.right.each(callback);
    }
  }
}
