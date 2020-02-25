/*
 * @lc app=leetcode id=147 lang=java
 *
 * [147] Insertion Sort List
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
    public ListNode insertionSortList(ListNode head) {
        ListNode tail = head, cur, tmp;
        while (tail != null && tail.next != null) {
            cur = tail.next;
            while (cur != null && cur.val >= tail.val) {
                tail = cur;
                cur = cur.next;
            }
            if (cur == null) break;
            
            tail.next = cur.next;
            if (cur.val <= head.val) {
                cur.next = head;
                head = cur;
            } else {
                tmp = head;
                while (tmp.next.val < cur.val) tmp = tmp.next;
                
                cur.next = tmp.next;
                tmp.next = cur;
            }
        }
        
        return head;
    }
}
// @lc code=end

