
// Importing classes
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Scanner;

public class SHA {
	
	public static void encrypt_string(String input)
	{
		try {
			
			MessageDigest md = MessageDigest.getInstance("SHA-1");

			// Calculating message digest and storing it in byte array
			byte[] messageDigest = md.digest(input.getBytes());

			// Converting byte array into signum representation
			BigInteger no = new BigInteger(1, messageDigest);

			// Converting message digest into hex value
			String hashtext = no.toString(16);

			// Adding preceding 0s to make it 32 bit
			while (hashtext.length() < 32) {
				hashtext = "0" + hashtext;
			}
			
			// Converting message digest into binary 
			String binarytext = no.toString(2);

			// Adding preceding 0s to make it 32 bit
			while (binarytext.length() < 32) {
				binarytext = "0" + binarytext;
			}

			System.out.println("\nMessage Digest in Hexadecimal format : " + hashtext);

			System.out.println("\nMessage Digest in Binary format : " + binarytext);
			
		}

		catch (NoSuchAlgorithmException e) {
			throw new RuntimeException(e);
		}
	}

	public static void main(String args[]) throws
									NoSuchAlgorithmException
	{

		System.out.println("Enter message : ");
        
		Scanner sc = new Scanner(System.in);	
		
		String msg = sc.nextLine();
		
		encrypt_string(msg);
		
		sc.close();
	}
}
