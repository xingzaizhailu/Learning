/*
 * @lc app=leetcode id=143 lang=java
 *
 * [143] Reorder List
 */

// @lc code=start
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode(int x) { val = x; }
 * }
 */
class Solution {
    public void reorderList(ListNode head) {
        if (head == null) return;
        ListNode fast = head.next, slow = head;
        while (fast != null && fast.next != null) {
            slow = slow.next;
            fast = fast.next.next;
        }
        
        ListNode tmp = slow;
        slow = slow.next;
        tmp.next = null;
        Stack<ListNode> stack = new Stack<>();
        while (slow != null) {
            stack.add(slow);
            slow = slow.next;
        }
        
        ListNode cur = head;
        while(!stack.isEmpty()) {
            tmp = stack.pop();
            tmp.next = cur.next;
            cur.next = tmp;
            cur = cur.next.next;
        }
        
    }
}
// @lc code=end

