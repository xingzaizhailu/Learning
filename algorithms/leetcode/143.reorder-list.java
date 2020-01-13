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
        if (head == null || head.next == null) return;
        ListNode fast = head.next, slow = head;
        while (fast != null && fast.next != null) {
            slow = slow.next;
            fast = fast.next.next;
        }
        
        // To reverse second half
        ListNode tmp = slow, secHalf = slow.next, prev = null;
        tmp.next = null;
        while (secHalf != null) {
            tmp = secHalf;
            secHalf = secHalf.next;
            tmp.next = prev;
            prev = tmp;
        }
        secHalf = prev;
        
        // Merge first half and the reversed second half
        ListNode cur = head;
        while(head != null && secHalf != null) {
            tmp = secHalf;
            secHalf = secHalf.next;
            tmp.next = cur.next;
            cur.next = tmp;
            cur = cur.next.next;
        }
    }
}
// @lc code=end

