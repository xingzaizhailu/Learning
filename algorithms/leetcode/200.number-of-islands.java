/*
 * @lc app=leetcode id=200 lang=java
 *
 * [200] Number of Islands
 */

// @lc code=start
class Solution {
    public int numIslands(char[][] grid) {
        int nIsland = 0;

        for (int i = 0; i < grid.length; ++i) {
            for (int j = 0; j < grid[0].length; ++j) {
                if (grid[i][j] != '1') continue;
                nIsland += 1;
                explore_island(grid, i, j);
            }
        }

        return nIsland;
    }
    
    private void explore_island(char[][] grid, int i, int j) {
        if (i < 0 || j < 0
            || i >= grid.length || j >= grid[0].length
            || grid[i][j] != '1')
            return;
        
        grid[i][j] = '#';
        explore_island(grid, i - 1, j);
        explore_island(grid, i, j - 1);
        explore_island(grid, i + 1, j);
        explore_island(grid, i, j + 1);
    }
}
// @lc code=end

