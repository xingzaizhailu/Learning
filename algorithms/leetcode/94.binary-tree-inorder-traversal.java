/*
 * @lc app=leetcode id=94 lang=java
 *
 * [94] Binary Tree Inorder Traversal
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
    public List<Integer> inorderTraversal(TreeNode root) {
        List<Integer> ret = new ArrayList<Integer>();
        _inorderTraversal(root, ret);
        return ret;
    }
    
    private void _inorderTraversal(TreeNode root, List<Integer> acc) {
        if (root == null) return;
        _inorderTraversal(root.left, acc);
        acc.add(root.val);
        _inorderTraversal(root.right, acc);
    }
}
// @lc code=end

