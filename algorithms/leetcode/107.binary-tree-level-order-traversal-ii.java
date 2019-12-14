/*
 * @lc app=leetcode id=107 lang=java
 *
 * [107] Binary Tree Level Order Traversal II
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
    Map<Integer, List<Integer>> map = new HashMap<Integer, List<Integer>>();
    
    public List<List<Integer>> levelOrderBottom(TreeNode root) {
        levelOrderBottom(root, 0);
        
        List<List<Integer>> ret = new ArrayList<List<Integer>>();
        int len = map.size();
        for (int i = len - 1; i >= 0; --i) {
            ret.add(map.get(i));
        }
        
        return ret;
    }
    
    private void levelOrderBottom(TreeNode root, int level) {
        if (root == null) return;
        List<Integer> lst = map.getOrDefault(level, new ArrayList<Integer>());
        lst.add(root.val);
        map.put(level, lst);
        levelOrderBottom(root.left, level + 1);
        levelOrderBottom(root.right, level + 1);
    }
}
// @lc code=end

