package aleryo;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.Length;

public class PaymentResult {

    private long id;
    private boolean paymentOk;

    public PaymentResult() {
        // Jackson deserialization
    }

    public PaymentResult(long id, boolean paymentOk) {
        this.id = id;
        this.paymentOk = paymentOk;
    }

    @JsonProperty
    public boolean getPaymentOk() {
        return paymentOk;
    }

    @JsonProperty
    public long getId() {
        return id;
    }

}
