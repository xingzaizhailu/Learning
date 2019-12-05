import java.util.ArrayList;
import java.util.List;

/*
 * @lc app=leetcode id=155 lang=java
 *
 * [155] Min Stack
 */

// @lc code=start
class MinStack {
    int min = Integer.MAX_VALUE;
    Stack<Integer> stack = null;

    /** initialize your data structure here. */
    public MinStack() { stack = new Stack<Integer>(); }
    
    public void push(int x) {
        if (x <= this.min) {
            stack.push(this.min);
            this.min = x; 
        }
        stack.push(x);
    }
    
    public void pop() {
        int tmp = stack.pop();
        if (tmp == this.min) {
            this.min = stack.pop();
        }
    }
    
    public int top() { return stack.peek(); }
    
    public int getMin() { return this.min; }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * MinStack obj = new MinStack();
 * obj.push(x);
 * obj.pop();
 * int param_3 = obj.top();
 * int param_4 = obj.getMin();
 */
// @lc code=end

