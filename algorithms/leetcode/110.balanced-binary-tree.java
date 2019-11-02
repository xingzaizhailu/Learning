/*
 * @lc app=leetcode id=110 lang=java
 *
 * [110] Balanced Binary Tree
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
    public boolean isBalanced(TreeNode root) {
        return getTreeDepth(root) >= 0 ? true : false;
    }

    private int getTreeDepth(TreeNode root) {
        if (root == null) return 0;

        int ldepth, rdepth;
        ldepth = getTreeDepth(root.left);
        rdepth = getTreeDepth(root.right);

        if (ldepth < 0) return ldepth;
        if (rdepth < 0) return rdepth;

        if (Math.abs(ldepth - rdepth) < 2) return Math.max(ldepth, rdepth) + 1;
        return -1;
    }
}
// @lc code=end

