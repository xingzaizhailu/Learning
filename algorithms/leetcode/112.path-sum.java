/*
 * @lc app=leetcode id=112 lang=java
 *
 * [112] Path Sum
 */

// @lc code=start
/**
 * Definition for a binary tree node.
 * public class TreeNode {
 *     int val;
 *     TreeNode left;
 *     TreeNode right;
 *     TreeNode(int x) { val = x; }
 * }
 */
class Solution {
    public boolean hasPathSum(TreeNode root, int sum) {
        return _hasPathSum(root, sum, 0);
    }
    
    private boolean _hasPathSum(TreeNode root, int sum, int acc) {
        if (root == null) return false;
        if (root.left == null && root.right == null) return sum == acc + root.val;
        
        return _hasPathSum(root.left, sum, acc + root.val)
            || _hasPathSum(root.right, sum, acc + root.val);
    }
}
// @lc code=end

