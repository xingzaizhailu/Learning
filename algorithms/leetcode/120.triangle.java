/*
 * @lc app=leetcode id=120 lang=java
 *
 * [120] Triangle
 */

// @lc code=start
class Solution {
    public int minimumTotal(List<List<Integer>> triangle) {
        if (triangle == null) return -1;
        List<Integer> row = triangle.get(triangle.size() - 1), rowBelow;
        
        for (int i = triangle.size() - 2; i >= 0; --i) {
            rowBelow = row;
            row = triangle.get(i);
            for (int j = 0; j < row.size(); ++j) {
                row.set(j, row.get(j) + Math.min(rowBelow.get(j), rowBelow.get(j+1)));
            }
            triangle.set(i, row);
        }
        return triangle.get(0).get(0);
    }
}
// @lc code=end

