package aleryo;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.Length;

public class Payment {

    private String cardNumber;

    public Payment() {
        // Jackson deserialization
    }

    public Payment(String cardNumber) {
        this.cardNumber = cardNumber;
    }


    public boolean checkCardNumber() {
        int sum = 0;
        boolean alternate = false;
        for (int i = cardNumber.length() - 1; i >= 0; i--)
            {
                int n = Integer.parseInt(cardNumber.substring(i, i + 1));
                if (alternate)
                    {
                        n *= 2;
                        if (n > 9)
                            {
                                n = (n % 10) + 1;
                            }
                    }
                sum += n;
                alternate = !alternate;
            }
        return (sum % 10 == 0);
    }

    @JsonProperty
    public String getCardNumber() {
        return cardNumber;
    }

}
