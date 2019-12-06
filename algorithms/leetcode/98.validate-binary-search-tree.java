/*
 * @lc app=leetcode id=98 lang=java
 *
 * [98] Validate Binary Search Tree
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
    Long last = Long.MIN_VALUE;
    
    public boolean isValidBST(TreeNode root) {
        boolean ret = true;
        if (root == null) return ret;
        
        ret &= isValidBST(root.left);
        ret &= this.last < Long.valueOf(root.val);
        this.last = Long.valueOf(root.val);
        ret &= isValidBST(root.right);
        
        return ret;
    }
}
// @lc code=end

